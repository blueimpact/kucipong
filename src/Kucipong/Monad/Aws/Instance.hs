{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.Aws.Instance where

import Kucipong.Prelude

import Control.Exception.Lens (_IOException, trying)
import Control.Lens ((&), (.~), view)
import Control.Monad.Trans.Resource (MonadResource)
import "cryptonite" Crypto.Hash (Digest, SHA256)
import Network.AWS
       (AWSRequest(..), HasEnv(..), _Error, runAWS, send, toBody)
import Network.AWS.Data (toText)
import Network.AWS.Data.Crypto (hashSHA256)
import Network.AWS.S3
       (ObjectCannedACL(OPublicRead), ObjectKey(..), putObject, poACL,
        poContentType)
import System.FilePath (replaceExtension, takeExtension)
import Web.Spock (UploadedFile(..))

import Kucipong.Aws
       (HasS3ImageBucketName(..), S3ImageBucketName(..), getAwsBucketM)
import Kucipong.Db (Image(..))
import Kucipong.Monad.Aws.Class
       (FileUploadError(..), MonadKucipongAws(..))
import Kucipong.Monad.Aws.Trans (KucipongAwsT(..))

instance ( HasEnv r
         , HasS3ImageBucketName r
         , MonadCatch m
         , MonadIO m
         , MonadReader r m
         , MonadResource m
         ) =>
         MonadKucipongAws (KucipongAwsT m) where
  awsS3PutUploadedFile :: UploadedFile
                       -> KucipongAwsT m (Either FileUploadError Image)
  awsS3PutUploadedFile uploadedFile =
    runExceptT $ do
      bucket <- getAwsBucketM
      contentType <- getContentType uploadedFile
      fileContents <- readUploadedFileContents uploadedFile
      let s3FileName = toS3FileName uploadedFile fileContents
          objectKey = ObjectKey s3FileName
          reqBody = toBody fileContents
      let putObjectReq =
            putObject bucket objectKey reqBody
              & poACL .~ Just OPublicRead
              & poContentType .~ Just contentType
      const (Image s3FileName) <$> runReq putObjectReq

  awsGetBucketName :: KucipongAwsT m S3ImageBucketName
  awsGetBucketName = reader getS3ImageBucketName


-- | Run an AWS request and return an 'Error'.
runReq
  :: forall r m a.
     ( AWSRequest a
     , HasEnv r
     , MonadError FileUploadError m
     , MonadReader r m
     , MonadResource m
     )
  => a -> m (Rs a)
runReq req = do
  awsEnv <- reader (view environment)
  eitherResult <- runAWS awsEnv . trying _Error $ send req
  case eitherResult of
    Left awsErr -> throwError $ AwsError awsErr
    Right res -> pure res

-- | Hash the contents of a 'ByteString'.
hashFileContents :: ByteString -> Digest SHA256
hashFileContents = hashSHA256

-- | Read the contents of an 'UploadedFile'.  Catches all 'IOException' and
-- rethrows them as 'FileReadError'.
readUploadedFileContents
  :: (MonadCatch m, MonadError FileUploadError m, MonadIO m)
  => UploadedFile -> m ByteString
readUploadedFileContents uploadedFile = do
  eitherFileContents <- trying _IOException . readFile $ uf_tempLocation uploadedFile
  case eitherFileContents of
    Left ioerr -> throwError $ FileReadError ioerr
    Right fileContents -> pure fileContents

-- | Calculates the new file name.  It becomes the SHA256 hash of the uploaded
-- file.  Also, take the file extension of the uploaded file and uses it for
-- the extension of the new file name.
toS3FileName :: UploadedFile -> ByteString -> Text
toS3FileName UploadedFile {uf_name} fileContents =
  let name = toText $ hashFileContents fileContents
  in pack . replaceExtension (unpack name) . takeExtension $ unpack uf_name

-- | Get the content type from a 'UploadedFile'.  If it is not an image file,
-- then throw a 'FileContentTypeError'.
getContentType :: MonadError FileUploadError m => UploadedFile -> m Text
getContentType uploadedfile =
  let contentType = uf_contentType uploadedfile
  in if contentTypeIsImage contentType
       then pure contentType
       else throwError FileContentTypeError

-- | Return true if the content type is the content type of an image.
contentTypeIsImage
  :: Text -- ^ Content type.  Something like @\"image/jpeg\"@ or
          -- @\"image/png\"@.
  -> Bool
contentTypeIsImage =
  (`elem` ["image/gif", "image/jpeg", "image/png", "image/svg+xml"])

{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.Aws.Instance where

import Kucipong.Prelude

import Control.Exception.Lens (_IOException, trying)
import Control.Lens ((&), (.~), view)
import Control.Monad.Trans.Resource (MonadResource)
import "cryptonite" Crypto.Hash (Digest, SHA256)
import Network.AWS
       (AWSRequest(..), Error, HasEnv(..), _Error, runAWS, send, toBody)
import Network.AWS.Data (toText)
import Network.AWS.Data.Crypto (hashSHA256)
import Network.AWS.S3
       (ObjectCannedACL(OPublicRead), ObjectKey(..), putObject, poACL)
import System.FilePath (replaceExtension, takeExtension)
import Web.Spock (UploadedFile(..))

import Kucipong.Aws (HasS3ImageBucketName, getAwsBucketM)
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
  awsS3PutUploadedFile uploadedFile = do
    bucket <- getAwsBucketM
    eitherFileContents <- readUploadedFileContents uploadedFile
    case eitherFileContents of
      Left exception -> pure . Left $ FileReadError exception
      Right fileContents -> do
        let s3FileName = toS3FileName uploadedFile fileContents
            objectKey = ObjectKey s3FileName
            reqBody = toBody fileContents
        let putObjectReq =
              putObject bucket objectKey reqBody & poACL .~ Just OPublicRead
        bimap AwsError (const $ Image s3FileName) <$> runReq putObjectReq

-- | Run an AWS request and return an 'Error'.
runReq
  :: (AWSRequest a, MonadResource m, HasEnv r, MonadReader r m)
  => a -> m (Either Error (Rs a))
runReq req = do
  awsEnv <- reader (view environment)
  runAWS awsEnv . trying _Error $ send req

-- | Hash the contents of a 'ByteString'.
hashFileContents :: ByteString -> Digest SHA256
hashFileContents = hashSHA256

-- | Read the contents of an 'UploadedFile'.  Catches all 'IOException'.
readUploadedFileContents
  :: (MonadCatch m, MonadIO m)
  => UploadedFile -> m (Either IOException ByteString)
readUploadedFileContents = trying _IOException . readFile . uf_tempLocation

-- | Calculates the new file name.  It becomes the SHA256 hash of the uploaded
-- file.  Also, take the file extension of the uploaded file and uses it for
-- the extension of the new file name.
toS3FileName :: UploadedFile -> ByteString -> Text
toS3FileName UploadedFile {uf_name} fileContents =
  let name = toText $ hashFileContents fileContents
  in pack . replaceExtension (unpack name) . takeExtension $ unpack uf_name

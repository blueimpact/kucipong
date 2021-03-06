{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Kucipong.Monad.Aws.Class where

import Kucipong.Prelude

import Control.Monad.Trans (MonadTrans)
import Data.Aeson (ToJSON(toJSON), Value(String))
import Network.AWS (Error)
import Web.Spock (ActionCtxT, UploadedFile)

import Kucipong.Aws (S3ImageBucketName(..))
import Kucipong.Db (ImageName(..))
import Kucipong.Monad.Cookie.Trans (KucipongCookieT)
import Kucipong.Monad.Db.Trans (KucipongDbT)
import Kucipong.Monad.SendEmail.Trans (KucipongSendEmailT)

data FileUploadError
  = AwsError Error
  | FileContentTypeError
  | FileReadError IOException
  deriving (Generic, Show, Typeable)

instance ToJSON FileUploadError where
  toJSON :: FileUploadError -> Value
  toJSON (AwsError _) = String "AwsError"
  toJSON FileContentTypeError = String "FileContentTypeError"
  toJSON (FileReadError _) = String "FileReadError"

-- |
-- Default implementations are used to easily derive instances for monads
-- transformers that implement 'MonadTrans'.
class Monad m => MonadKucipongAws m where
  awsS3PutUploadedFile
    :: UploadedFile
    -> m (Either FileUploadError ImageName)
  default awsS3PutUploadedFile
    :: ( MonadKucipongAws n
       , MonadTrans t
       , m ~ t n
       )
    => UploadedFile -> t n (Either FileUploadError ImageName)
  awsS3PutUploadedFile = lift . awsS3PutUploadedFile

  awsGetBucketName :: m S3ImageBucketName
  default awsGetBucketName
    :: ( MonadKucipongAws n
       , MonadTrans t
       , m ~ t n
       )
    => t n S3ImageBucketName
  awsGetBucketName = lift awsGetBucketName

instance MonadKucipongAws m => MonadKucipongAws (ActionCtxT ctx m)
instance MonadKucipongAws m => MonadKucipongAws (ExceptT e m)
instance MonadKucipongAws m => MonadKucipongAws (IdentityT m)
instance MonadKucipongAws m => MonadKucipongAws (KucipongCookieT m)
instance MonadKucipongAws m => MonadKucipongAws (KucipongDbT m)
instance MonadKucipongAws m => MonadKucipongAws (KucipongSendEmailT m)
instance MonadKucipongAws m => MonadKucipongAws (ReaderT r m)

awsImageS3Url :: MonadKucipongAws m => ImageName -> m Text
awsImageS3Url image = do
  bucketName <- awsGetBucketName
  pure $ awsUrlFromImageAndBucket bucketName image

awsUrlFromImageAndBucket :: S3ImageBucketName -> ImageName -> Text
awsUrlFromImageAndBucket (S3ImageBucketName bucketName) (ImageName s3FileName) =
  "https://s3-ap-northeast-1.amazonaws.com/" <> bucketName <> "/" <> s3FileName

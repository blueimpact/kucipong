{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Kucipong.Aws

Helper functions for working with S3.
-}

module Kucipong.Aws
    ( HasAwsRegion(..)
    , S3ImageBucketName(..)
    , HasS3ImageBucketName(..)
    , createS3ImageBucket
    , getAwsBucketM
    ) where

import Kucipong.Prelude

import Control.Exception.Lens (trying)
import Control.Lens ((&), set)
import Control.Monad.Trans.Resource (runResourceT)
import Network.AWS
       (Env, Error(ServiceError), Region(..),
        ServiceError(ServiceError', _serviceStatus), _Error, runAWS, send)
import Network.AWS.Data (ToText(toText))
import Network.AWS.S3
       (BucketName(..), LocationConstraint(..),
        cbCreateBucketConfiguration, cbcLocationConstraint, createBucket,
        createBucketConfiguration)
import Network.HTTP.Types (Status(Status, statusCode))

-- | Newtype wrapper around the S3 image bucket name.
newtype S3ImageBucketName = S3ImageBucketName
  { unS3ImageBucketName :: Text
  } deriving (Data, Eq, Generic, IsString, Ord, Show, Typeable)

class HasS3ImageBucketName r where
  getS3ImageBucketName :: r -> S3ImageBucketName

instance HasS3ImageBucketName S3ImageBucketName where
  getS3ImageBucketName :: S3ImageBucketName -> S3ImageBucketName
  getS3ImageBucketName = id

class HasAwsRegion r where
  getAwsRegion :: r -> Region

instance HasAwsRegion Region where
  getAwsRegion :: Region -> Region
  getAwsRegion = id

-- | Given a 'Region' and an 'Env', try to create a bucket on S3 with the name
-- 'S3ImageBucketName'.
--
-- Succeed if either the bucket has been successfully created, or the bucket
-- already exists.
--
-- Fail with 'error' if the bucket could not be created.  This could happen for
-- many reasons.  For example, because of no network connectivity.
createS3ImageBucket
  :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadThrow m)
  => Region -> Env -> S3ImageBucketName -> m ()
createS3ImageBucket awsRegion awsEnv s3ImageBucketName = do
  let bucketName = s3ImageBucketNameToBucketName s3ImageBucketName
      bucketConf =
        createBucketConfiguration &
        set cbcLocationConstraint (Just (LocationConstraint awsRegion))
      createBucketReq =
        createBucket bucketName &
        set cbCreateBucketConfiguration (Just bucketConf)
  eitherCreateBucketResp <-
    runResourceT . runAWS awsEnv . trying _Error $ send createBucketReq
  case eitherCreateBucketResp of
    Right createBucketResp -> do
      $(logDebug) $
        "successfully created bucket \"" <> toText bucketName <> "\" on aws: " <>
        tshow createBucketResp
      pure ()
    -- The bucket already exists, so we don't need to do anything.
    Left errResp@(ServiceError ServiceError' {_serviceStatus = Status {statusCode = 409}}) -> do
      $(logDebug) $
        "bucket \"" <> toText bucketName <>
        "\" already exists. No action needs to be taken: " <>
        tshow errResp
      pure ()
    Left errResp -> do
      error $
        "got unexpected error when trying to create bucket \"" <>
        unpack (toText bucketName) <>
        "\" on aws: " <>
        show errResp

-- | Convert an 'S3ImageBucketName' to a 'BucketName'.
s3ImageBucketNameToBucketName :: S3ImageBucketName -> BucketName
s3ImageBucketNameToBucketName = BucketName . unS3ImageBucketName

getAwsBucketM :: (HasS3ImageBucketName r, MonadReader r m) => m BucketName
getAwsBucketM = reader $ s3ImageBucketNameToBucketName . getS3ImageBucketName

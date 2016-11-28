{- |
Module      :  Kucipong.Aws

Helper functions for working with S3.
-}

module Kucipong.Aws
    ( HasAwsRegion(..)
    , S3ImageBucketName(..)
    , HasS3ImageBucketName(..)
    , createS3ImageBucket
    ) where

import Kucipong.Prelude

import Control.Exception.Lens (trying)
import Control.Lens ((&), set)
import Control.Monad.Trans.Resource (runResourceT)
import Network.AWS (Env, Region(..), _Error, runAWS, send)
import Network.AWS.S3
       (BucketName(..), LocationConstraint(..),
        cbCreateBucketConfiguration, cbcLocationConstraint, createBucket,
        createBucketConfiguration)

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

createS3ImageBucket :: Region -> Env -> S3ImageBucketName -> IO ()
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
      Right createBucketResp -> pure ()
      Left errorResp ->
        -- TODO: throw an error here if we are not able to create the bucket on S3.
        pure ()

s3ImageBucketNameToBucketName :: S3ImageBucketName -> BucketName
s3ImageBucketNameToBucketName = BucketName . unS3ImageBucketName

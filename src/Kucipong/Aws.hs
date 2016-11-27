{- |
Module      :  Kucipong.Aws

Helper functions for working with S3.
-}

module Kucipong.Aws
    ( S3ImageBucketName(..)
    , HasS3ImageBucketName(..)
    , createS3ImageBucket
    ) where

import Kucipong.Prelude

import Control.Lens (Prism', (&), _Just, set)
import Control.Monad.Trans.Resource (runResourceT)
import Network.AWS (Env, Region(..), runAWS, send)
import Network.AWS.S3
       (BucketName(..), CreateBucket, LocationConstraint(..),
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

createS3ImageBucket :: Env -> S3ImageBucketName -> IO ()
createS3ImageBucket awsEnv s3ImageBucketName = do
  let bucketName = s3ImageBucketNameToBucketName s3ImageBucketName
      bucketConf =
        createBucketConfiguration &
        set cbcLocationConstraint (Just (LocationConstraint Tokyo))
      createBucketReq =
        createBucket bucketName &
        set cbCreateBucketConfiguration (Just bucketConf)
  createBucketResp <- runResourceT . runAWS awsEnv $ send createBucketReq
  print "resp:"
  print createBucketResp
  print "(end resp)"
  pure ()

s3ImageBucketNameToBucketName :: S3ImageBucketName -> BucketName
s3ImageBucketNameToBucketName = BucketName . unS3ImageBucketName

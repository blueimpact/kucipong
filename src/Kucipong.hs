
module Kucipong ( defaultMain ) where

import Kucipong.Prelude

import Database.Persist.Sql (runSqlPool)

import Kucipong.Aws (createS3ImageBucket)
import Kucipong.Config
       (Config(..), createConfigFromEnv, setLoggerMiddleware)
import Kucipong.Db (doMigrations)
import Kucipong.Handler (app)
import Kucipong.Logger (runLogger)

-- | Run the API.
defaultMain :: IO ()
defaultMain = do
  config <- createConfigFromEnv
  runLogger $
    createS3ImageBucket
      (configAwsRegion config)
      (configAwsEnv config)
      (configS3ImageBucketName config)
  runSqlPool doMigrations $ configPool config
  let loggerMiddleware = setLoggerMiddleware config
  app loggerMiddleware config

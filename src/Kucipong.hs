
module Kucipong ( defaultMain ) where

import Kucipong.Prelude

import Database.Persist.Sql ( runSqlPool )

import Kucipong.Aws (createS3ImageBucket)
import Kucipong.Config ( Config(..), createConfigFromEnv, setLoggerMiddleware )
import Kucipong.Db ( doMigrations )
import Kucipong.Handler ( app )

-- | Run the API.
defaultMain :: IO ()
defaultMain = do
    config <- createConfigFromEnv
    createS3ImageBucket
      (configAwsRegion config)
      (configAwsEnv config)
      (configS3ImageBucketName config)
    -- TODO: Probably shouldn't run migrations in production automatically.
    runSqlPool doMigrations $ configPool config
    let loggerMiddleware = setLoggerMiddleware config
    app loggerMiddleware config

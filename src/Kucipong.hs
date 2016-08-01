
module Kucipong ( defaultMain ) where

import Kucipong.Prelude

import Kucipong.Config ( createConfigFromEnv, getPort )
import Kucipong.Db ( {- doMigrations -} )
import Kucipong.Handler ( app )

-- | Run the API.
defaultMain :: IO ()
defaultMain = do
    config <- createConfigFromEnv
    -- Run migrations.
    -- TODO: Probably shouldn't run migrations in production automatically.
    -- runSqlPool doMigrations $ configPool config
    putStrLn $ "kucipong app running on port " <> tshow (getPort config) <> "..."
    app config

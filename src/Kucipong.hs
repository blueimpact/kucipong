
module Kucipong ( defaultMain ) where

import Kucipong.Prelude

import Network.Wai.Handler.Warp ( Port, run )
import Database.Persist.Postgresql ( runSqlPool )

import Kucipong.Config ( Config(..), createConfigFromEnv, getPort, setLogger )
import Kucipong.Environment ( Environment(..) )
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

module Kucipong.Config
    ( module Kucipong.Config
    , Config(..)
    ) where

import Kucipong.Prelude

import Control.Monad.Logger ( runStdoutLoggingT )
import Database.Persist.Postgresql ( ConnectionPool )
import Database.PostgreSQL.Simple ( ConnectInfo(..) )
import Mail.Hailgun ( HailgunContext(..) )
import Network.HTTP.Client ( Manager, newManager )
import Network.HTTP.Client.Conduit ( HasHttpManager(..) )
import Network.HTTP.Conduit (tlsManagerSettings)
import Network.Wai.Handler.Warp ( Port )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev, logStdout )
import Network.Wai ( Middleware )
import System.ReadEnvVar ( lookupEnvDef, readEnvVarDef )

import Kucipong.Environment ( Environment(..), HasEnv(..) )
import Kucipong.Email ( HasHailgunContext(..) )
import Kucipong.Db
    ( DbPoolConnNum, DbPoolConnTimeout, HasDbPool(..), makePool )

-- | A 'Config' used by our application.  It contains things used
-- throughout a request.
data Config = Config
    { configEnv  :: Environment
    , configHailgunContext :: HailgunContext
    , configHttpManager :: Manager
    , configPool :: ConnectionPool
    , configPort :: Port
    }

class HasPort a where
    getPort :: a -> Port

instance HasDbPool Config where
    getDbPool :: Config -> ConnectionPool
    getDbPool = configPool

instance HasEnv Config where
    getEnv :: Config -> Environment
    getEnv = configEnv

instance HasHailgunContext Config where
    getHailgunContext :: Config -> HailgunContext
    getHailgunContext = configHailgunContext

instance HasHttpManager Config where
    getHttpManager :: Config -> Manager
    getHttpManager = configHttpManager

instance HasPort Config where
    getPort :: Config -> Port
    getPort = configPort

-- | Returns a 'Middleware' with our logger.
setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

-- | This represents a URL to the kucipong service.
--
-- This should probably be a 'Network.HTTP.Client.Request' type instead of
-- 'Text'.  If it is 'Request', the path and querystring can be added used the
-- 'Network.HTTP.Client.path' and 'Network.HTTP.Client.querystring' methods
-- as <http://www.yesodweb.com/book/settings-types described here>.
kucipongBaseRequest :: Environment -> Text
kucipongBaseRequest Test = "http://www.kucipong.com/"
kucipongBaseRequest Development = "http://127.0.0.1:8091/"
kucipongBaseRequest Production = "https://www.kucipong.com/"

kucipongHost :: Environment -> Text
kucipongHost Test = "127.0.0.1"
kucipongHost Development = "127.0.0.1"
kucipongHost Production = "kucipong.com"

createConfigFromEnv :: IO Config
createConfigFromEnv = do
    env <- readEnvVarDef "KUCIPONG_ENV" Development
    port <- readEnvVarDef "KUCIPONG_PORT" 8101
    hailgunContextDomain <- lookupEnvDef "KUCIPONG_MAILGUN_DOMAIN" "sandboxfaf3e17ba66f42a5ac5cd36e8a71ad97.mailgun.org"
    hailgunContextApiKey <- lookupEnvDef "KUCIPONG_MAILGUN_APIKEY" "todo-fake-apikey"
    dbConnNum <- readEnvVarDef "KUCIPONG_DB_CONN_NUM" 10
    dbConnTimeout <- fromInteger <$> readEnvVarDef "KUCIPONG_DB_CONN_TIMEOUT" 60 -- 1 minute
    dbHost <- lookupEnvDef "KUCIPONG_DB_HOST" "localhost"
    dbPort <- readEnvVarDef "KUCIPONG_DB_PORT" 5432
    dbUser <- lookupEnvDef "KUCIPONG_DB_USER" "kucipong"
    dbPass <- lookupEnvDef "KUCIPONG_DB_PASSWORD" "nuy07078akyy1y7anvya7072"
    dbDatabase <- lookupEnvDef "KUCIPONG_DB_DATABASE" "kucipong"
    createConfigFromValues env port hailgunContextDomain hailgunContextApiKey
        dbConnNum dbConnTimeout dbHost dbPort dbUser dbPass dbDatabase

type DbHost = String
type DbPort = Word16
type DbUser = String
type DbPassword = String
type DbName = String

type HailgunDomain = String
type HailgunApiKey = String

createConfigFromValues
    :: Environment
    -> Port
    -> HailgunDomain
    -> HailgunApiKey
    -> DbPoolConnNum
    -> DbPoolConnTimeout
    -> DbHost
    -> DbPort
    -> DbUser
    -> DbPassword
    -> DbName
    -> IO Config
createConfigFromValues env port hailgunContextDomain hailgunContextApiKey
        dbConnNum dbConnTimeout dbHost dbPort dbUser dbPass dbName = do
    httpManager <- newManager tlsManagerSettings
    let hailgunContext = HailgunContext
            { hailgunDomain = hailgunContextDomain
            , hailgunApiKey = hailgunContextApiKey
            , hailgunProxy = Nothing
            }
    let dbConnInfo = ConnectInfo
            { connectHost = dbHost
            , connectPort = dbPort
            , connectUser = dbUser
            , connectPassword = dbPass
            , connectDatabase = dbName
            }
    pool <- runStdoutLoggingT $ makePool dbConnInfo dbConnTimeout dbConnNum
    pure Config
        { configPool = pool
        , configEnv = env
        , configHailgunContext = hailgunContext
        , configHttpManager = httpManager
        , configPort = port
        }

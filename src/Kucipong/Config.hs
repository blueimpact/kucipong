module Kucipong.Config
    ( module Kucipong.Config
    , Config(..)
    ) where

import Kucipong.Prelude

import Control.FromSum ( fromEitherM )
import Control.Monad.Logger ( runStdoutLoggingT )
import Data.ByteString.Base64 ( decode )
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
import Web.ClientSession ( Key, initKey )

import Kucipong.Db
    ( DbPoolConnNum, DbPoolConnTimeout, HasDbPool(..), makePool )
import Kucipong.Environment ( Environment(..), HasEnv(..) )
import Kucipong.Email ( HasHailgunContext(..) )
import Kucipong.Host
    ( HasHost(..), HasPort(..), HasProtocol(..), Host, Protocol )
import Kucipong.Session ( HasSessionKey(..) )

-- | A 'Config' used by our application.  It contains things used
-- throughout a request.
data Config = Config
    { configEnv  :: Environment
    , configHailgunContext :: HailgunContext
    , configHost :: Text
    , configHttpManager :: Manager
    , configPool :: ConnectionPool
    , configPort :: Port
    , configProtocol :: Text
    , configSessionKey :: Key
    }

instance HasDbPool Config where
    getDbPool :: Config -> ConnectionPool
    getDbPool = configPool

instance HasEnv Config where
    getEnv :: Config -> Environment
    getEnv = configEnv

instance HasHailgunContext Config where
    getHailgunContext :: Config -> HailgunContext
    getHailgunContext = configHailgunContext

instance HasHost Config where
    getHost :: Config -> Text
    getHost = configHost

instance HasHttpManager Config where
    getHttpManager :: Config -> Manager
    getHttpManager = configHttpManager

instance HasPort Config where
    getPort :: Config -> Port
    getPort = configPort

instance HasProtocol Config where
    getProtocol :: Config -> Text
    getProtocol = configProtocol

instance HasSessionKey Config where
    getSessionKey :: Config -> Key
    getSessionKey = configSessionKey

-- | Returns a 'Middleware' with our logger.
setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

-- | This is the key for encrypting session data.  A new key for use in
-- production can be created like the following:
--
-- @
--   import Data.ByteString.Base64 ( 'encode' )
--   import Web.ClientSession ( 'randomKey' )
--
--   newKey :: IO 'ByteString'
--   newKey =
--       (byteString, _) <- 'randomKey'
--       'encode' byteString
-- @
kucipongSessionKeyDev :: ByteString
kucipongSessionKeyDev =
    "lCNWb2gFVE8QtvV+dqjmYMWK6aq1Y9vQ5PmJb0ZMiZ5AG6G9zp+bJY8aficESqo+uX" <>
    "+UEbhQN5dUqQXSEk0H8F/FGLUGywKCvnw8e7UcPx5rgK7xCdeGLJXm8R4B2ihK"

-- | Initialize a Session 'Key' out of a base64-encoded 'ByteString'.  Throws
-- an error if they input key is either not base64-encoded, or the
-- base64-decoded key is not exactly 96 bytes.
initKucipongSessionKey :: ByteString -> IO Key
initKucipongSessionKey = fromEitherM handleErr . (initKey <=< decode)
  where
    handleErr :: String -> a
    handleErr err =
        error $ "error with decoding session key: " <> err

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
    sessionKeyRaw <- lookupEnvDef "KUCIPONG_SESSION_KEY" kucipongSessionKeyDev
    sessionKey <- initKucipongSessionKey sessionKeyRaw
    host <- lookupEnvDef "KUCIPONG_HOST" "localhost:8101"
    protocol <- lookupEnvDef "KUCIPONG_PROTOCOL" "http"
    createConfigFromValues env port hailgunContextDomain hailgunContextApiKey
        dbConnNum dbConnTimeout dbHost dbPort dbUser dbPass dbDatabase
        sessionKey host protocol

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
    -> Key
    -> Host
    -> Protocol
    -> IO Config
createConfigFromValues env port hailgunContextDomain hailgunContextApiKey
        dbConnNum dbConnTimeout dbHost dbPort dbUser dbPass dbName
        sessionKey host protocol = do
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
        , configHost = host
        , configPort = port
        , configProtocol = protocol
        , configSessionKey = sessionKey
        }

module Kucipong.Config
    ( module Kucipong.Config
    , Config(..)
    ) where

import Kucipong.Prelude

import Control.FromSum (fromEitherM, fromEitherOrM)
import Control.Lens (Lens', lens, set)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.ByteString.Base64 (decode)
import Database.Persist.Postgresql (ConnectionPool)
import Database.PostgreSQL.Simple (ConnectInfo(..))
import Mail.Hailgun (HailgunContext(..))
import Network.AWS
       (AccessKey, Credentials(FromKeys), Env, LogLevel(Debug), Region,
        SecretKey, envLogger, newEnv, newLogger)
import qualified Network.AWS as AWS
import Network.AWS.Data (fromText)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.Conduit (HasHttpManager(..))
import Network.HTTP.Conduit (tlsManagerSettings)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger
       (logStdoutDev, logStdout)
import System.ReadEnvVar (lookupEnvDef, readEnvVarDef)
import Web.ClientSession (Key, initKey)

import Kucipong.Aws
       (HasAwsRegion(..), HasS3ImageBucketName(..), S3ImageBucketName(..))
import Kucipong.Db
       (DbPoolConnNum, DbPoolConnTimeout, HasDbPool(..), makePool)
import Kucipong.Email (HasHailgunContext(..))
import Kucipong.Environment (Environment(..), HasEnv(..))
import Kucipong.Host
       (HasHost(..), HasPort(..), HasProtocol(..), Host, Protocol)
import Kucipong.Session (HasSessionKey(..))

-- | A 'Config' used by our application.  It contains things used
-- throughout a request.
data Config = Config
  { configAwsEnv :: Env
  , configAwsRegion :: Region
  , configEnv :: Environment
  , configHailgunContext :: HailgunContext
  , configHost :: Text
  , configHttpManager :: Manager
  , configPool :: ConnectionPool
  , configPort :: Port
  , configProtocol :: Text
  , configS3ImageBucketName :: S3ImageBucketName
  , configSessionKey :: Key
  }

instance HasAwsRegion Config where
  getAwsRegion :: Config -> Region
  getAwsRegion = configAwsRegion

instance HasDbPool Config where
  getDbPool :: Config -> ConnectionPool
  getDbPool = configPool

instance AWS.HasEnv Config where
  environment :: Lens' Config Env
  environment =
    lens configAwsEnv (\config newAwsEnv -> config {configAwsEnv = newAwsEnv})

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

instance HasS3ImageBucketName Config where
  getS3ImageBucketName :: Config -> S3ImageBucketName
  getS3ImageBucketName = configS3ImageBucketName

instance HasSessionKey Config where
  getSessionKey :: Config -> Key
  getSessionKey = configSessionKey

-- | Returns a 'Middleware' with our logger.
setLoggerMiddleware
  :: (HasEnv r)
  => r -> Middleware
setLoggerMiddleware r =
  case getEnv r of
    Test -> id
    Development -> logStdoutDev
    Production -> logStdout

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
    handleErr err = error $ "error with decoding session key: " <> err

createConfigFromEnv :: IO Config
createConfigFromEnv = do
  env <- readEnvVarDef "KUCIPONG_ENV" Development
  port <- readEnvVarDef "PORT" 8101
  hailgunContextDomain <-
    lookupEnvDef
      "KUCIPONG_MAILGUN_DOMAIN"
      "sandboxfaf3e17ba66f42a5ac5cd36e8a71ad97.mailgun.org"
  hailgunContextApiKey <-
    lookupEnvDef "KUCIPONG_MAILGUN_APIKEY" "todo-fake-apikey"
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
  eitherAwsRegion <- fromText <$> lookupEnvDef "KUCIPONG_AWS_REGION" "ap-northeast-1"
  awsRegion <- fromEitherOrM eitherAwsRegion $ \err ->
    error $ "Couldn't convert the region string into an actual region: " <> err
  awsAccessKey <-
    lookupEnvDef "KUCIPONG_AWS_ACCESS_KEY" "todo-fake-aws-access-key"
  awsSecretKey <-
    lookupEnvDef "KUCIPONG_AWS_SECRET_KEY" "todo-fake-aws-secret-key"
  awsS3ImageBucketName <-
    lookupEnvDef "KUCIPONG_S3_IMAGE_BUCKET_NAME" "kucipong-images-dev"
  createConfigFromValues
    env
    port
    hailgunContextDomain
    hailgunContextApiKey
    dbConnNum
    dbConnTimeout
    dbHost
    dbPort
    dbUser
    dbPass
    dbDatabase
    sessionKey
    host
    protocol
    awsRegion
    awsAccessKey
    awsSecretKey
    awsS3ImageBucketName

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
  -> Region
  -> AccessKey
  -> SecretKey
  -> S3ImageBucketName
  -> IO Config
createConfigFromValues env port hailgunContextDomain hailgunContextApiKey
    dbConnNum dbConnTimeout dbHost dbPort dbUser dbPass dbName sessionKey host
    protocol awsRegion awsAccessKey awsSecretKey s3ImageBucketName = do
  httpManager <- newManager tlsManagerSettings
  let hailgunContext =
        HailgunContext
        { hailgunDomain = hailgunContextDomain
        , hailgunApiKey = hailgunContextApiKey
        , hailgunProxy = Nothing
        }
  awsEnv <- newEnv awsRegion (FromKeys awsAccessKey awsSecretKey)
  awsEnvPlusLogger <-
    case env of
      Development -> do
        awsLogger <- newLogger Debug stdout
        pure $ set envLogger awsLogger awsEnv
      _ -> pure awsEnv
  let dbConnInfo =
        ConnectInfo
        { connectHost = dbHost
        , connectPort = dbPort
        , connectUser = dbUser
        , connectPassword = dbPass
        , connectDatabase = dbName
        }
  pool <- runStdoutLoggingT $ makePool dbConnInfo dbConnTimeout dbConnNum
  pure
    Config
    { configAwsEnv = awsEnvPlusLogger
    , configAwsRegion = awsRegion
    , configEnv = env
    , configHailgunContext = hailgunContext
    , configHost = host
    , configHttpManager = httpManager
    , configPool = pool
    , configPort = port
    , configProtocol = protocol
    , configS3ImageBucketName = s3ImageBucketName
    , configSessionKey = sessionKey
    }

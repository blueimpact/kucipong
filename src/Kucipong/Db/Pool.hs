
module Kucipong.Db.Pool where

import Kucipong.Prelude

import Data.Pool ( createPool )
import Data.Time.Clock ( NominalDiffTime )
import Database.Persist.Postgresql ( openSimpleConn )
import Database.Persist.Sql
    ( ConnectionPool, LogFunc, SqlBackend, askLogFunc, close' )
import Database.PostgreSQL.Simple ( ConnectInfo(..), connect)

-- | Class for things that have a persistent 'ConnectionPool'.
class HasDbPool a where
    getDbPool :: a -> ConnectionPool

type DbPoolConnNum = Int
type DbPoolConnTimeout = NominalDiffTime

makePool
    :: forall m
     . ( MonadBaseControl IO m
       , MonadIO m
       , MonadLogger m
       )
    => ConnectInfo
    -> DbPoolConnTimeout
    -> DbPoolConnNum
    -> m ConnectionPool
makePool dbConnInfo dbConnTimeout dbConnNum = do
    logFunc <- askLogFunc
    liftIO $ createPool (getConn logFunc) close' 1 dbConnTimeout dbConnNum
  where
    getConn :: LogFunc -> IO SqlBackend
    getConn logFunc = do
        conn <- connect dbConnInfo
        openSimpleConn logFunc conn

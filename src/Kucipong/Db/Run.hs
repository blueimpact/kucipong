
module Kucipong.Db.Run where

import Kucipong.Prelude

import Database.Persist.Postgresql
       (SqlBackend(..), runMigration, runSqlPool)
import Database.PostgreSQL.Simple (SqlError(..))

import Kucipong.Db.Pool (HasDbPool(..))
import Kucipong.Db.Models (migrateAll)

-- | Run all sql migration.
doMigrations :: (MonadIO m) => ReaderT SqlBackend m ()
doMigrations = runMigration migrateAll

-- | Run a Persistent query.
runDb
  :: (MonadIO m, MonadReader r m, HasDbPool r, MonadBaseControl IO m)
  => ReaderT SqlBackend m a -> m a
runDb query = reader getDbPool >>= runSqlPool query

data DbSafeError
  = DbSafeUniquenessViolation
  | DbSafeOtherError
  deriving (Data, Eq, Generic, Show, Typeable)

-- | Function to convert the generic 'SqlError' to an actual 'DbSafeError' that
-- the end user can easily work with.
sqlErrorToDbSafeError :: SqlError -> DbSafeError
sqlErrorToDbSafeError sqlError
  | isUniquenessViolation sqlError = DbSafeUniquenessViolation
  | otherwise = DbSafeOtherError
  where
    -- Return True if an 'SqlError' is a uniqueness violation.
    isUniquenessViolation :: SqlError -> Bool
    isUniquenessViolation SqlError {sqlErrorMsg} =
      "duplicate key value violates unique constraint" `isInfixOf` sqlErrorMsg

-- | Just like 'runDB' but catch SQL uniqueness violation errors and
-- rethrow them as more specific Kucipong errors.
runDbSafe
  :: forall m r a.
     ( MonadCatch m
     , MonadIO m
     , MonadReader r m
     , HasDbPool r
     , MonadBaseControl IO m
     )
  => ReaderT SqlBackend m a -> m (Either DbSafeError a)
runDbSafe query = first sqlErrorToDbSafeError <$> try (runDb query)

-- | Just like 'runDb' but provide the current time to the callback.
runDbCurrTime
  :: (MonadIO m, MonadReader r m, HasDbPool r, MonadBaseControl IO m)
  => (UTCTime -> ReaderT SqlBackend m a) -> m a
runDbCurrTime query = do
  currentTime <- liftIO getCurrentTime
  runDb $ query currentTime

-- | Combination of 'runDbCurrTime' and 'runDbSafe'.
runDbSafeCurrTime
  :: ( MonadCatch m
     , MonadIO m
     , MonadReader r m
     , HasDbPool r
     , MonadBaseControl IO m
     )
  => (UTCTime -> ReaderT SqlBackend m a) -> m (Either DbSafeError a)
runDbSafeCurrTime query = runDbSafe . query =<< liftIO getCurrentTime

{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.Db.Instance where

import Kucipong.Prelude

import Control.Monad.Random (MonadRandom(..))
import Control.Monad.Time (MonadTime(..))
import Database.Persist.Sql
       (BaseBackend, Entity(..), PersistEntity, PersistEntityBackend,
        PersistStoreRead, SqlBackend, (==.), (=.), get, insert,
        insertEntity, repsert, selectFirst, updateGet)

import Kucipong.Config (Config)
import Kucipong.Db
       (Admin(..), AdminLoginToken(..), CreatedTime(..), DbSafeError(..),
        EntityField(..), Image, Key(..), LoginTokenExpirationTime(..),
        Store(..), StoreEmail(..), StoreLoginToken(..), UpdatedTime(..),
        runDb, runDbCurrTime, runDbSafe)
import Kucipong.LoginToken (LoginToken, createRandomLoginToken)
import Kucipong.Monad.Db.Class (MonadKucipongDb(..))
import Kucipong.Monad.Db.Trans (KucipongDbT(..))
import Kucipong.Persist (repsertEntity)
import Kucipong.Util (addOneDay)

instance ( MonadBaseControl IO m
         , MonadCatch m
         , MonadIO m
         , MonadRandom m
         , MonadReader Config m
         , MonadTime m
         ) =>
         MonadKucipongDb (KucipongDbT m) where

    -- ===========
    --  For Admin
    -- ===========
  dbCreateAdmin :: EmailAddress -> Text -> KucipongDbT m (Entity Admin)
  dbCreateAdmin email name = lift go
    where
      go :: m (Entity Admin)
      go = do
        currTime <- currentTime
        let admin =
              Admin
                email
                (CreatedTime currTime)
                (UpdatedTime currTime)
                Nothing
                name
        adminKey <- runDb $ insert admin
        pure $ Entity adminKey admin

  dbCreateAdminMagicLoginToken :: Key Admin
                               -> KucipongDbT m (Entity AdminLoginToken)
  dbCreateAdminMagicLoginToken adminKey = lift go
    where
      go :: m (Entity AdminLoginToken)
      go = do
        currTime <- currentTime
        randomLoginToken <- createRandomLoginToken
        let plusOneDay = addOneDay currTime
        let newAdminLoginTokenVal =
              AdminLoginToken
                adminKey
                (CreatedTime currTime)
                (UpdatedTime currTime)
                Nothing
                randomLoginToken
                (LoginTokenExpirationTime plusOneDay)
        runDb $ repsertEntity (AdminLoginTokenKey adminKey) newAdminLoginTokenVal

  dbFindAdminLoginToken :: LoginToken
                        -> KucipongDbT m (Maybe (Entity AdminLoginToken))
  dbFindAdminLoginToken loginToken = lift go
    where
      go :: m (Maybe (Entity AdminLoginToken))
      go = runDb $ selectFirst [AdminLoginTokenLoginToken ==. loginToken] []

  dbFindAdmin :: EmailAddress -> KucipongDbT m (Maybe (Entity Admin))
  dbFindAdmin email = lift go
    where
      go :: m (Maybe (Entity Admin))
      go = fmap (fmap createEntity) . runDb $ get adminKey

      createEntity :: Admin -> Entity Admin
      createEntity = Entity adminKey

      adminKey :: Key Admin
      adminKey = AdminKey email

  dbUpsertAdmin :: EmailAddress -> Text -> KucipongDbT m (Entity Admin)
  dbUpsertAdmin email name = lift go
    where
      go :: m (Entity Admin)
      go =
        runDbCurrTime $ \currTime -> do
          maybeExistingAdminVal <- get (AdminKey email)
          case maybeExistingAdminVal of
            -- admin already exists.  update the name if it is different
            Just existingAdminVal -> do
              if (adminName existingAdminVal /= name)
                then do
                  newAdminVal <- updateGet (AdminKey email) [AdminName =. name]
                  pure $ Entity (AdminKey email) newAdminVal
                else pure $ Entity (AdminKey email) existingAdminVal
            -- couldn't find an existing admin, so we will create a new
            -- one
            Nothing -> do
              let newAdminVal =
                    Admin
                      email
                      (CreatedTime currTime)
                      (UpdatedTime currTime)
                      Nothing
                      name
              newAdminKey <- insert newAdminVal
              pure $ Entity newAdminKey newAdminVal

  -- ===========
  --  For Store
  -- ===========
  dbCreateStore
    :: Key StoreEmail
      -- ^ 'Key' for the 'StoreEmail'
    -> Text
      -- ^ 'Store' name
    -> Text
      -- ^ 'Store' category
    -> Text
      -- ^ 'Store' category detail
    -> Maybe Image
      -- ^ 'Image' for the 'Store'
    -> Maybe Text
      -- ^ Sales Point for the 'Store'
    -> Maybe Text
      -- ^ Address for the 'Store'
    -> Maybe Text
      -- ^ Phone number for the 'Store'
    -> Maybe Text
      -- ^ Business hours for the 'Store'
    -> Maybe Text
      -- ^ Regular holiday for the 'Store'
    -> Maybe Text
      -- ^ url for the 'Store'
    -> KucipongDbT m (Entity Store)
  dbCreateStore storeEmailKey name category catdet image salesPoint address phoneNumber
          businessHours regularHoliday url = lift go
    where
      go :: m (Entity Store)
      go = do
          currTime <- currentTime
          let store =
                  Store storeEmailKey (CreatedTime currTime)
                      (UpdatedTime currTime) Nothing name category catdet
                      image salesPoint address phoneNumber businessHours
                      regularHoliday url
          runDb $ repsertEntity (StoreKey storeEmailKey) store

  dbCreateStoreEmail :: EmailAddress
                     -> KucipongDbT m (Either DbSafeError (Entity StoreEmail))
  dbCreateStoreEmail email = lift go
    where
      go :: m (Either DbSafeError (Entity StoreEmail))
      go = do
        currTime <- currentTime
        let storeEmail =
              StoreEmail
                email
                (CreatedTime currTime)
                (UpdatedTime currTime)
                Nothing
        runDbSafe $ insertEntity storeEmail

  dbCreateStoreMagicLoginToken :: Key StoreEmail
                               -> KucipongDbT m (Entity StoreLoginToken)
  dbCreateStoreMagicLoginToken storeEmailKey = lift go
    where
      go :: m (Entity StoreLoginToken)
      go = do
        currTime <- currentTime
        randomLoginToken <- createRandomLoginToken
        let plusOneDay = addOneDay currTime
        let newStoreLoginTokenVal =
              StoreLoginToken
                storeEmailKey
                (CreatedTime currTime)
                (UpdatedTime currTime)
                Nothing
                randomLoginToken
                (LoginTokenExpirationTime plusOneDay)
        runDb $ repsert (StoreLoginTokenKey storeEmailKey) newStoreLoginTokenVal
        pure $ Entity (StoreLoginTokenKey storeEmailKey) newStoreLoginTokenVal

  dbFindStoreLoginToken :: LoginToken -> KucipongDbT m (Maybe (Entity StoreLoginToken))
  dbFindStoreLoginToken loginToken = lift go
    where
      go :: m (Maybe (Entity StoreLoginToken))
      go = runDb $ selectFirst [StoreLoginTokenLoginToken ==. loginToken] []

  -- ======= --
  -- Generic --
  -- ======= --

  dbFindByKey
    :: forall record.
       (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
    => Key record -> KucipongDbT m (Maybe (Entity record))
  dbFindByKey key = lift go
    where
      go :: m (Maybe (Entity record))
      go = runDb $ getEntity key


-----------
-- Store --
-----------

dbFindStoreByEmail
  :: MonadKucipongDb m
  => EmailAddress -> m (Maybe (Entity Store))
dbFindStoreByEmail = dbFindByKey . StoreKey . StoreEmailKey

-------------
-- Helpers --
-------------

getEntity
  :: ( MonadIO m
     , PersistEntity record
     , PersistEntityBackend record ~ BaseBackend backend
     , PersistStoreRead backend
     )
  => Key record -> ReaderT backend m (Maybe (Entity record))
getEntity key = fmap (Entity key) <$> get key

{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.Db.Instance where

import Kucipong.Prelude

import Control.Monad.Random (MonadRandom(..))
import Control.Monad.Time (MonadTime(..))
import Database.Persist.Sql
       (Entity(..), Filter, PersistRecordBackend, PersistStoreRead,
        SelectOpt, SqlBackend, (==.), (=.), get, insert, insertEntity,
        repsert, selectFirst, selectList, update, updateGet)

import Kucipong.Config (Config)
import Kucipong.Db
       (Admin(..), AdminLoginToken(..), CreatedTime(..), DeletedTime(..),
        DbSafeError(..), EntityField(..), EntityDateFields(..), Image,
        Key(..), LoginTokenExpirationTime(..), Store(..), StoreEmail(..),
        StoreLoginToken(..), UpdatedTime(..), emailToAdminKey,
        emailToStoreEmailKey, emailToStoreKey, runDb, runDbCurrTime,
        runDbSafe)
import Kucipong.LoginToken (LoginToken, createRandomLoginToken)
import Kucipong.Monad.Db.Class
       (MonadKucipongDb(..), StoreDeleteResult(..))
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
    -> [Text]
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
  dbCreateStore storeEmailKey name category catdets image salesPoint address phoneNumber
          businessHours regularHoliday url = lift go
    where
      go :: m (Entity Store)
      go = do
          currTime <- currentTime
          let store =
                  Store storeEmailKey (CreatedTime currTime)
                      (UpdatedTime currTime) Nothing name category catdets
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

  dbDeleteStoreIfNameMatches
    :: EmailAddress
    -> Text
    -> KucipongDbT m StoreDeleteResult
  dbDeleteStoreIfNameMatches email name = lift go
    where
      go :: m StoreDeleteResult
      go =
        runDbCurrTime $ \currTime -> do
          let storeKey = emailToStoreKey email
          maybeStore <- get storeKey
          case maybeStore of
            Just store
              | storeName store == name -> do
                update storeKey [StoreDeleted =. Just (DeletedTime currTime)]
                pure StoreDeleteSuccess
              | otherwise -> pure $ StoreDeleteErrNameDoesNotMatch store
            Nothing -> pure StoreDeleteErrDoesNotExist


  -- ======= --
  -- Generic --
  -- ======= --

  dbFindByKey
    :: forall record.
       (PersistRecordBackend record SqlBackend)
    => Key record -> KucipongDbT m (Maybe (Entity record))
  dbFindByKey key = lift go
    where
      go :: m (Maybe (Entity record))
      go = runDb $ getEntity key

  dbSelectFirst
    :: forall record.
       (PersistRecordBackend record SqlBackend)
    => [Filter record]
    -> [SelectOpt record]
    -> KucipongDbT m (Maybe (Entity record))
  dbSelectFirst filters selectOpts = lift go
    where
      go :: m (Maybe (Entity record))
      go = runDb $ selectFirst filters selectOpts

  dbSelectList
    :: forall record.
       (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [SelectOpt record] -> KucipongDbT m [Entity record]
  dbSelectList filters selectOpts = lift go
    where
      go :: m [Entity record]
      go = runDb $ selectList filters selectOpts

  dbUpsert
    :: forall record.
       (PersistRecordBackend record SqlBackend)
    => Key record
    -> (UTCTime -> Maybe record -> record)
    -> KucipongDbT m (Entity record)
  dbUpsert key recordCreator = lift go
    where
      go :: m (Entity record)
      go =
        runDbCurrTime $ \currTime -> do
          maybeVal <- get key
          let newRecord = recordCreator currTime maybeVal
          repsertEntity key newRecord

-------------
-- Generic --
-------------

dbFindByKeyNotDeleted
  :: forall m record.
     ( EntityDateFields record
     , MonadKucipongDb m
     , PersistRecordBackend record SqlBackend
     )
  => Key record -> m (Maybe (Entity record))
dbFindByKeyNotDeleted key = do
  maybeEntity <- dbFindByKey key
  pure $
    maybeEntity >>= \(Entity _ value) ->
      case getDeletedEntityFieldValue value of
        -- If this entity is not deleted, then just return the entity.
        Nothing -> pure (Entity key value)
        -- If this entity is deleted, then return Nothing.
        Just _ -> Nothing

dbSelectFirstNotDeleted
  :: forall m record.
     ( EntityDateFields record
     , MonadKucipongDb m
     , PersistRecordBackend record SqlBackend
     )
  => [Filter record] -> [SelectOpt record] -> m (Maybe (Entity record))
dbSelectFirstNotDeleted filters selectOpts =
  dbSelectFirst ((deletedEntityField ==. Nothing) : filters) selectOpts

dbSelectListNotDeleted
  :: forall m record.
     ( EntityDateFields record
     , MonadKucipongDb m
     , PersistRecordBackend record SqlBackend
     )
  => [Filter record] -> [SelectOpt record] -> m [Entity record]
dbSelectListNotDeleted filters selectOpts =
  dbSelectList ((deletedEntityField ==. Nothing) : filters) selectOpts

dbUpsertWithTime
  :: forall m record.
     ( EntityDateFields record
     , MonadKucipongDb m
     , PersistRecordBackend record SqlBackend
     )
  => Key record
  -> (CreatedTime -> UpdatedTime -> Maybe DeletedTime -> record)
  -> m (Entity record)
dbUpsertWithTime key recordCreator =
  dbUpsert key $ \currTime ->
    \case
      Nothing ->
        recordCreator (CreatedTime currTime) (UpdatedTime currTime) Nothing
      Just record ->
        let createdTime = getCreatedEntityFieldValue record
            deletedTime = getDeletedEntityFieldValue record
        in recordCreator createdTime (UpdatedTime currTime) deletedTime

-----------
-- Admin --
-----------

dbFindAdminLoginToken
  :: MonadKucipongDb m
  => LoginToken -> m (Maybe (Entity AdminLoginToken))
dbFindAdminLoginToken loginToken =
  dbSelectFirstNotDeleted [AdminLoginTokenLoginToken ==. loginToken] []

dbFindAdmin
  :: MonadKucipongDb m
  => EmailAddress -> m (Maybe (Entity Admin))
dbFindAdmin = dbFindByKeyNotDeleted . emailToAdminKey

-----------
-- Store --
-----------

dbFindStoreByEmail
  :: MonadKucipongDb m
  => EmailAddress -> m (Maybe (Entity Store))
dbFindStoreByEmail = dbFindByKeyNotDeleted . StoreKey . StoreEmailKey

dbFindStoreLoginToken
  :: MonadKucipongDb m
  => LoginToken -> m (Maybe (Entity StoreLoginToken))
dbFindStoreLoginToken loginToken =
  dbSelectFirstNotDeleted [StoreLoginTokenLoginToken ==. loginToken] []

dbUpsertStore
  :: MonadKucipongDb m
  => EmailAddress
  -> Text
  -> Text
  -> [Text]
  -> Maybe Image
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> m (Entity Store)
dbUpsertStore email name businessCategory businessCategoryDetails image salesPoint address phoneNumber businessHours regularHoliday url =
  dbUpsertWithTime (emailToStoreKey email) $ \createdTime updatedTime deletedTime ->
    Store
      (emailToStoreEmailKey email)
      createdTime
      updatedTime
      deletedTime
      name
      businessCategory
      businessCategoryDetails
      image
      salesPoint
      address
      phoneNumber
      businessHours
      regularHoliday
      url

-------------
-- Helpers --
-------------

getEntity
  :: ( MonadIO m
     , PersistRecordBackend record backend
     , PersistStoreRead backend
     )
  => Key record -> ReaderT backend m (Maybe (Entity record))
getEntity key = fmap (Entity key) <$> get key

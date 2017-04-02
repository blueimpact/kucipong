{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.Db.Instance where

import Kucipong.Prelude

import Control.Monad.Random (MonadRandom(..))
import Control.Monad.Time (MonadTime(..))
import Database.Persist.Sql
       (Entity(..), Filter, PersistRecordBackend, PersistStoreRead,
        SelectOpt, SqlBackend, Update, (==.), (=.), (>=.), (<=.), (||.),
        get, insert, insertEntity, insertUnique, repsert, selectFirst,
        selectList, update, updateGet, updateWhere)

import Kucipong.Config (Config)
import Kucipong.Db
       (Admin(..), AdminLoginToken(..), BusinessCategory(..),
        BusinessCategoryDetail(..), Coupon(..), CouponType(..),
        CreatedTime(..), DeletedTime(..), EntityField(..),
        EntityDateFields(..), Image, Key(..), LoginTokenExpirationTime(..),
        Percent(..), Price(..), Store(..), StoreLoginToken(..),
        UpdatedTime(..), emailToAdminKey, runDb, runDbCurrTime)
import Kucipong.LoginToken (LoginToken, createRandomLoginToken)
import Kucipong.Monad.Db.Class
       (CouponDeleteResult(..), MonadKucipongDb(..),
        StoreDeleteResult(..))
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

    -- =======
    --  Admin
    -- =======
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

  -- =======
  --  Store
  -- =======
  dbCreateStoreMagicLoginToken :: Key Store
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

  dbCreateInitStore :: EmailAddress -> KucipongDbT m (Maybe (Entity Store))
  dbCreateInitStore email = lift go
    where
      go :: m (Maybe (Entity Store))
      go = do
        currTime <- currentTime
        runDb $ do
          maybeStore <- selectFirst [StoreEmail ==. email, StoreDeleted ==. Nothing] []
          case maybeStore of
            Just _ -> pure Nothing
            Nothing -> do
              let newStore =
                    Store
                      email
                      (CreatedTime currTime)
                      (UpdatedTime currTime)
                      Nothing
                      Nothing
                      Nothing
                      []
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
              maybeKey <- insertUnique newStore
              pure $ fmap (\key -> Entity key newStore) maybeKey

  dbDeleteStoreIfNameMatches
    :: EmailAddress
    -> Text
    -> KucipongDbT m StoreDeleteResult
  dbDeleteStoreIfNameMatches email name = lift go
    where
      go :: m StoreDeleteResult
      go =
        runDbCurrTime $ \currTime -> do
          maybeStoreEntity <- selectFirst [StoreEmail ==. email, StoreDeleted ==. Nothing] []
          case maybeStoreEntity of
            Nothing -> pure $ StoreDeleteErrDoesNotExist email
            Just (Entity storeKey store) ->
              deleteBasedOnName (storeName store) name storeKey store currTime

      -- | Delete the store after comparing the name of the store from the
      -- database to the name the user passed in.
      --
      -- If the store name from the database is 'Nothing', and the name passed
      -- in by the user is @(no store name)@, then delete the store.
      --
      -- If the store name from the database is 'Nothing', and the name passed
      -- in by the user is @\"\"@, then delete the store.
      --
      -- If the store name from the database is 'Nothing', and the name passed
      -- in by the user is not @\"\"@, then return
      -- 'StoreDeleteErrNameDoesNotMatch'.
      --
      -- If the store name from the database is 'Just', and the name matches
      -- the name entered by the user, then delete the store.
      --
      -- If the store name from the database is 'Just', and it does not match
      -- the name entered by the user, then return
      -- 'StoreDeleteErrNameDoesNotMatch'.
      deleteBasedOnName
        :: Maybe Text
        -- ^ Store name from the database.
        -> Text
        -- ^ Store name entered by the user.
        -> Key Store
        -> Store
        -> UTCTime
        -- ^ Current time.
        -> ReaderT SqlBackend m StoreDeleteResult
      deleteBasedOnName Nothing "(no store name)" storeKey _ currTime =
        doDelete storeKey currTime
      deleteBasedOnName Nothing "" storeKey _ currTime =
        doDelete storeKey currTime
      deleteBasedOnName Nothing nameFromUser _ store _ =
        pure $ StoreDeleteErrNameDoesNotMatch store nameFromUser
      deleteBasedOnName (Just nameFromDb) nameFromUser storeKey store currTime
        | nameFromDb == nameFromUser =
          doDelete storeKey currTime
        | otherwise =
          pure $ StoreDeleteErrNameDoesNotMatch store nameFromUser

      doDelete :: Key Store -> UTCTime -> ReaderT SqlBackend m StoreDeleteResult
      doDelete storeKey currTime = do
        update
          storeKey
          [ StoreDeleted =. Just (DeletedTime currTime)
          , StoreUpdated =. UpdatedTime currTime
          ]
        pure StoreDeleteSuccess

  -- ========
  --  Coupon
  -- ========

  dbDeleteCoupon :: Key Store -> Key Coupon -> KucipongDbT m CouponDeleteResult
  dbDeleteCoupon storeKey couponKey = lift go
    where
      go :: m CouponDeleteResult
      go =
        runDbCurrTime $ \currTime -> do
          maybeCouponEntity <-
            selectFirst [CouponId ==. couponKey, CouponStoreId ==. storeKey] []
          case maybeCouponEntity of
            Nothing -> pure $ CouponDeleteErrDoesNotExist
            Just _ -> do
              update
                couponKey
                [ CouponDeleted =. Just (DeletedTime currTime)
                , CouponUpdated =. UpdatedTime currTime
                ]
              pure $ CouponDeleteSuccess

  -- =========
  --  Generic
  -- =========

  dbFindByKey
    :: forall record.
       (PersistRecordBackend record SqlBackend)
    => Key record -> KucipongDbT m (Maybe (Entity record))
  dbFindByKey key = lift go
    where
      go :: m (Maybe (Entity record))
      go = runDb $ getEntity key

  dbInsert
    :: forall record.
       (PersistRecordBackend record SqlBackend)
    => (UTCTime -> record)
    -> KucipongDbT m (Entity record)
  dbInsert recordCreator = lift go
    where
      go :: m (Entity record)
      go =
        runDbCurrTime $ \currTime -> do
          let newRecord = recordCreator currTime
          insertEntity newRecord

  dbInsertUnique
    :: forall record.
       (PersistRecordBackend record SqlBackend)
    => (UTCTime -> record)
    -> KucipongDbT m (Maybe (Entity record))
  dbInsertUnique recordCreator = lift go
    where
      go :: m (Maybe (Entity record))
      go =
        runDbCurrTime $ \currTime -> do
          let newRecord = recordCreator currTime
          maybeKey <- insertUnique newRecord
          pure $ fmap (\key -> Entity key newRecord) maybeKey

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

  dbUpdate
    :: forall record.
       (PersistRecordBackend record SqlBackend)
    => [Filter record] -> (UTCTime -> [Update record]) -> KucipongDbT m ()
  dbUpdate filters updatesCreator = lift go
    where
      go :: m ()
      go = runDbCurrTime $ updateWhere filters . updatesCreator

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

dbInsertWithTime
  :: forall m record.
     ( MonadKucipongDb m
     , PersistRecordBackend record SqlBackend
     )
  => (CreatedTime -> UpdatedTime -> Maybe DeletedTime -> record)
  -> m (Entity record)
dbInsertWithTime recordCreator =
  dbInsert $ \currTime ->
    recordCreator (CreatedTime currTime) (UpdatedTime currTime) Nothing

dbInsertUniqueWithTime
  :: forall m record.
     ( MonadKucipongDb m
     , PersistRecordBackend record SqlBackend
     )
  => (CreatedTime -> UpdatedTime -> Maybe DeletedTime -> record)
  -> m (Maybe (Entity record))
dbInsertUniqueWithTime recordCreator =
  dbInsertUnique $ \currTime ->
    recordCreator (CreatedTime currTime) (UpdatedTime currTime) Nothing

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

dbUpdateWithTime
  :: forall m record.
     ( EntityDateFields record
     , MonadKucipongDb m
     , PersistRecordBackend record SqlBackend
     )
  => [Filter record] -> [Update record] -> m ()
dbUpdateWithTime filters updates = dbUpdate filters $ \currTime ->
  (updatedEntityField =. UpdatedTime currTime) : updates

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

dbFindStoreByStoreKey
  :: MonadKucipongDb m
  => Key Store -> m (Maybe (Entity Store))
dbFindStoreByStoreKey = dbFindByKeyNotDeleted

dbFindStoreByEmail
  :: MonadKucipongDb m
  => EmailAddress -> m (Maybe (Entity Store))
dbFindStoreByEmail email = dbSelectFirstNotDeleted [StoreEmail ==. email] []

dbFindStoreLoginToken
  :: MonadKucipongDb m
  => LoginToken -> m (Maybe (Entity StoreLoginToken))
dbFindStoreLoginToken loginToken =
  dbSelectFirstNotDeleted [StoreLoginTokenLoginToken ==. loginToken] []

dbUpdateStore
  :: MonadKucipongDb m
  => Key Store
  -> Maybe Text
  -> Maybe BusinessCategory
  -> [BusinessCategoryDetail]
  -> Maybe Image
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> m ()
dbUpdateStore storeKey name businessCategory businessCategoryDetails image salesPoint address phoneNumber businessHours regularHoliday url =
  dbUpdateWithTime
    [StoreId ==. storeKey]
    [ StoreName =. name
    , StoreBusinessCategory =. businessCategory
    , StoreBusinessCategoryDetails =. businessCategoryDetails
    , StoreImage =. image
    , StoreSalesPoint =. salesPoint
    , StoreAddress =. address
    , StorePhoneNumber =. phoneNumber
    , StoreBusinessHours =. businessHours
    , StoreRegularHoliday =. regularHoliday
    , StoreUrl =. url
    ]

------------
-- Coupon --
------------

dbInsertCoupon
  :: MonadKucipongDb m
  => Key Store
  -> Text
  -> CouponType
  -> Maybe Day
  -> Maybe Day
  -> Maybe Percent
  -> Maybe Price
  -> Maybe Text
  -> Maybe Text
  -> Maybe Price
  -> Maybe Price
  -> Maybe Text
  -> Maybe Text
  -> Maybe Price
  -> Maybe Price
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> m (Entity Coupon)
dbInsertCoupon storeKey title couponType validFrom validUntil discountPercent discountMinimumPrice discountOtherConditions giftContent giftReferencePrice giftMinimumPrice giftOtherConditions setContent setPrice setReferencePrice setOtherConditions otherContent otherConditions =
  dbInsertWithTime $ \createdTime updatedTime deletedTime ->
    Coupon
      storeKey
      createdTime
      updatedTime
      deletedTime
      title
      couponType
      validFrom
      validUntil
      Nothing
      discountPercent
      discountMinimumPrice
      discountOtherConditions
      giftContent
      giftReferencePrice
      giftMinimumPrice
      giftOtherConditions
      setContent
      setPrice
      setReferencePrice
      setOtherConditions
      otherContent
      otherConditions

dbFindCouponByStoreKeyAndCouponKey
  :: MonadKucipongDb m
  => Key Store -> Key Coupon -> m (Maybe (Entity Coupon))
dbFindCouponByStoreKeyAndCouponKey storeKey couponKey =
  dbSelectFirstNotDeleted
    [CouponStoreId ==. storeKey, CouponId ==. couponKey]
    []

dbFindCouponsByStoreKey
  :: MonadKucipongDb m
  => Key Store -> m [Entity Coupon]
dbFindCouponsByStoreKey storeKey =
  dbSelectListNotDeleted [CouponStoreId ==. storeKey] []

dbUpdateCoupon
  :: MonadKucipongDb m
  => Key Coupon
  -> Key Store
  -> Text
  -> CouponType
  -> Maybe Day
  -> Maybe Day
  -> Maybe Percent
  -> Maybe Price
  -> Maybe Text
  -> Maybe Text
  -> Maybe Price
  -> Maybe Price
  -> Maybe Text
  -> Maybe Text
  -> Maybe Price
  -> Maybe Price
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> m ()
dbUpdateCoupon couponKey storeKey title couponType validFrom validUntil discountPercent discountMinimumPrice discountOtherConditions giftContent giftReferencePrice giftMinimumPrice giftOtherConditions setContent setPrice setReferencePrice setOtherConditions otherContent otherConditions =
  dbUpdateWithTime
    [CouponId ==. couponKey, CouponStoreId ==. storeKey]
    [ CouponTitle =. title
    , CouponCouponType =. couponType
    , CouponValidFrom =. validFrom
    , CouponValidUntil =. validUntil
    , CouponDiscountPercent =. discountPercent
    , CouponDiscountMinimumPrice =. discountMinimumPrice
    , CouponDiscountOtherConditions =. discountOtherConditions
    , CouponGiftContent =. giftContent
    , CouponGiftReferencePrice =. giftReferencePrice
    , CouponGiftMinimumPrice =. giftMinimumPrice
    , CouponGiftOtherConditions =. giftOtherConditions
    , CouponSetContent =. setContent
    , CouponSetPrice =. setPrice
    , CouponSetReferencePrice =. setReferencePrice
    , CouponSetOtherConditions =. setOtherConditions
    , CouponOtherContent =. otherContent
    , CouponOtherConditions =. otherConditions
    ]

--------------
-- Consumer --
--------------

dbFindPublicCouponById
  :: forall m.
     (MonadKucipongDb m, MonadIO m)
  => Key Coupon -> m (Maybe (Entity Coupon))
dbFindPublicCouponById couponKey = do
  today <- liftIO (utctDay <$> getCurrentTime)
  dbSelectFirstNotDeleted
    ([CouponId ==. couponKey] ++
     ([CouponValidFrom <=. Just today] ||. [CouponValidFrom ==. Nothing]) ++
     ([CouponValidUntil >=. Just today] ||. [CouponValidUntil ==. Nothing]))
    []

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

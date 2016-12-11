{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Kucipong.Monad.Db.Class where

import Kucipong.Prelude

import Control.Monad.Trans (MonadTrans)
import Database.Persist.Sql
       (Entity, Filter, PersistRecordBackend, SelectOpt, SqlBackend)
import Web.Spock (ActionCtxT)

import Kucipong.Db
       (Admin, AdminLoginToken, BusinessCategory(..),
        BusinessCategoryDetail(..), DbSafeError, Image, Key, Store,
        StoreEmail, StoreLoginToken)
import Kucipong.Monad.Cookie.Trans (KucipongCookieT)
import Kucipong.Monad.SendEmail.Trans (KucipongSendEmailT)

-- | Result to return from a call to 'dbDeleteStoreIfNameMatches'.
data StoreDeleteResult
  = StoreDeleteSuccess
  -- ^ Successfully deleted the store.
  | StoreDeleteErrNameDoesNotMatch Store Text
  -- ^ 'Store' name passed in does not match the name of the actual 'Store'.
  | StoreDeleteErrDoesNotExist EmailAddress
  -- ^ 'EmailAddress' passed in does not match an actual 'Store'.
  deriving (Eq, Generic, Show, Typeable)

-- | Type-class for monads that can perform Db actions.  For instance, querying
-- the database for information or writing new information to the database.
--
-- Default implementations are used to easily derive instances for monads
-- transformers that implement 'MonadTrans'.
class Monad m => MonadKucipongDb m where

  -- ===========
  --  For Admin
  -- ===========

  dbCreateAdmin
      :: EmailAddress
      -> Text
      -- ^ Admin name
      -> m (Entity Admin)
  default dbCreateAdmin
      :: ( MonadKucipongDb n
         , MonadTrans t
         , m ~ t n
         )
      => EmailAddress -> Text -> t n (Entity Admin)
  dbCreateAdmin = (lift .) . dbCreateAdmin

  dbCreateAdminMagicLoginToken :: Key Admin -> m (Entity AdminLoginToken)
  default dbCreateAdminMagicLoginToken
      :: ( MonadKucipongDb n
         , MonadTrans t
         , m ~ t n
         )
      => Key Admin -> t n (Entity AdminLoginToken)
  dbCreateAdminMagicLoginToken = lift . dbCreateAdminMagicLoginToken

  dbUpsertAdmin
      :: EmailAddress
      -> Text
      -- ^ Admin name
      -> m (Entity Admin)
  default dbUpsertAdmin
      :: ( MonadKucipongDb n
         , MonadTrans t
         , m ~ t n
         )
      => EmailAddress -> Text -> t n (Entity Admin)
  dbUpsertAdmin = (lift .) . dbUpsertAdmin

  -- ===========
  --  For Store
  -- ===========

  dbCreateStore
      :: Key StoreEmail
      -- ^ 'Key' for the 'StoreEmail'
      -> Text
      -- ^ 'Store' name
      -> BusinessCategory
      -- ^ 'Store' category
      -> [BusinessCategoryDetail]
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
      -> m (Entity Store)
  default dbCreateStore
      :: ( MonadKucipongDb n
         , MonadTrans t
         , m ~ t n
         )
      => Key StoreEmail -> Text -> BusinessCategory -> [BusinessCategoryDetail] -> Maybe Image
      -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text
      -> Maybe Text -> Maybe Text -> t n (Entity Store)
  dbCreateStore email name category catdet image salesPoint address phoneNumber
          businessHours regularHoliday url = lift $
      dbCreateStore
          email
          name
          category
          catdet
          image
          salesPoint
          address
          phoneNumber
          businessHours
          regularHoliday
          url

  dbCreateStoreEmail :: EmailAddress -> m (Either DbSafeError (Entity StoreEmail))
  default dbCreateStoreEmail
      :: ( MonadKucipongDb n
         , MonadTrans t
         , m ~ t n
         )
      => EmailAddress -> t n (Either DbSafeError (Entity StoreEmail))
  dbCreateStoreEmail = lift . dbCreateStoreEmail

  dbCreateStoreMagicLoginToken :: Key StoreEmail -> m (Entity StoreLoginToken)
  default dbCreateStoreMagicLoginToken
      :: ( MonadKucipongDb n
         , MonadTrans t
         , m ~ t n
         )
      => Key StoreEmail -> t n (Entity StoreLoginToken)
  dbCreateStoreMagicLoginToken = lift . dbCreateStoreMagicLoginToken

  dbDeleteStoreIfNameMatches
    :: EmailAddress
    -- ^ 'Key' for the 'Store'
    -> Text
    -- ^ Name of the 'Store'.
    -> m StoreDeleteResult
  default dbDeleteStoreIfNameMatches
    :: ( MonadKucipongDb n
       , MonadTrans t
       , m ~ t n
       )
    => EmailAddress -> Text -> t n StoreDeleteResult
  dbDeleteStoreIfNameMatches email name =
    lift $ dbDeleteStoreIfNameMatches email name

  -- ======= --
  -- Generic --
  -- ======= --
  dbFindByKey
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> m (Maybe (Entity record))
  default dbFindByKey
    :: ( MonadKucipongDb n
       , MonadTrans t
       , m ~ t n
       , PersistRecordBackend record SqlBackend
       )
    => Key record -> t n (Maybe (Entity record))
  dbFindByKey = lift . dbFindByKey

  dbSelectFirst
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [SelectOpt record] -> m (Maybe (Entity record))
  default dbSelectFirst
    :: ( MonadKucipongDb n
       , MonadTrans t
       , m ~ t n
       , PersistRecordBackend record SqlBackend
       )
    => [Filter record] -> [SelectOpt record] -> t n (Maybe (Entity record))
  dbSelectFirst filters selectOpts = lift (dbSelectFirst filters selectOpts)

  dbSelectList
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [SelectOpt record] -> m [Entity record]
  default dbSelectList
    :: ( MonadKucipongDb n
       , MonadTrans t
       , m ~ t n
       , PersistRecordBackend record SqlBackend
       )
    => [Filter record] -> [SelectOpt record] -> t n [Entity record]
  dbSelectList filters selectOpts = lift (dbSelectList filters selectOpts)

  dbUpsert
    :: (PersistRecordBackend record SqlBackend)
    => Key record
    -> (UTCTime -> Maybe record -> record)
    -> m (Entity record)
  default dbUpsert
    :: ( MonadKucipongDb n
       , MonadTrans t
       , m ~ t n
       , PersistRecordBackend record SqlBackend
       )
    => Key record
    -> (UTCTime -> Maybe record -> record)
    -> t n (Entity record)
  dbUpsert key recordCreator = lift (dbUpsert key recordCreator)

instance MonadKucipongDb m => MonadKucipongDb (ActionCtxT ctx m)
instance MonadKucipongDb m => MonadKucipongDb (ExceptT e m)
instance MonadKucipongDb m => MonadKucipongDb (IdentityT m)
instance MonadKucipongDb m => MonadKucipongDb (KucipongCookieT m)
instance MonadKucipongDb m => MonadKucipongDb (KucipongSendEmailT m)
instance MonadKucipongDb m => MonadKucipongDb (ReaderT r m)

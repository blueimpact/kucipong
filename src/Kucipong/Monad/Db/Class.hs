{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Kucipong.Monad.Db.Class where

import Kucipong.Prelude

import Control.Monad.Trans (MonadTrans)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Database.Persist.Sql
       (Entity, Filter, PersistRecordBackend, SelectOpt, SqlBackend,
        Update)
import Web.Spock (ActionCtxT)

import Kucipong.Db
       (Admin, AdminLoginToken, Coupon, Key, Store, StoreLoginToken)
import Kucipong.Monad.Aws.Trans ( KucipongAwsT )
import Kucipong.Monad.Cookie.Trans (KucipongCookieT)
import Kucipong.Monad.SendEmail.Trans (KucipongSendEmailT)

-- | Result to return from a call to 'dbDeleteCoupon'.
data CouponDeleteResult
  = CouponDeleteSuccess
  -- ^ Successfully deleted the coupon.
  | CouponDeleteErrDoesNotExist
  -- ^ There is no 'Coupon' in the database for the 'Key' 'Coupon' and 'Key'
  -- 'Store' that was passed in.
  deriving (Eq, Generic, Show, Typeable)

$(deriveJSON defaultOptions ''CouponDeleteResult)

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

  -- =======
  --  Admin
  -- =======

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

  -- =======
  --  Store
  -- =======

  dbCreateStoreMagicLoginToken :: Key Store -> m (Entity StoreLoginToken)
  default dbCreateStoreMagicLoginToken
      :: ( MonadKucipongDb n
         , MonadTrans t
         , m ~ t n
         )
      => Key Store -> t n (Entity StoreLoginToken)
  dbCreateStoreMagicLoginToken = lift . dbCreateStoreMagicLoginToken

  dbCreateInitStore :: EmailAddress -> m (Maybe (Entity Store))
  default dbCreateInitStore
      :: ( MonadKucipongDb n
         , MonadTrans t
         , m ~ t n
         )
      => EmailAddress -> t n (Maybe (Entity Store))
  dbCreateInitStore = lift . dbCreateInitStore

  dbDeleteStoreIfNameMatches
    :: EmailAddress
    -- ^ 'EmailAddress' for the 'Store'
    -> Text
    -- ^ Name of the 'Store'.
    -> m StoreDeleteResult
  default dbDeleteStoreIfNameMatches
    :: ( MonadKucipongDb n
       , MonadTrans t
       , m ~ t n
       )
    => EmailAddress -> Text -> t n StoreDeleteResult
  dbDeleteStoreIfNameMatches storeKey name =
    lift $ dbDeleteStoreIfNameMatches storeKey name

  -- ========
  --  Coupon
  -- ========

  dbDeleteCoupon :: Key Store -> Key Coupon -> m CouponDeleteResult
  default dbDeleteCoupon
    :: ( MonadKucipongDb n
       , MonadTrans t
       , m ~ t n
       )
    => Key Store -> Key Coupon -> t n CouponDeleteResult
  dbDeleteCoupon storeKey = lift . dbDeleteCoupon storeKey

  -- =========
  --  Generic
  -- =========

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

  dbInsert
    :: (PersistRecordBackend record SqlBackend)
    => (UTCTime -> record)
    -> m (Entity record)
  default dbInsert
    :: ( MonadKucipongDb n
       , MonadTrans t
       , m ~ t n
       , PersistRecordBackend record SqlBackend
       )
    => (UTCTime -> record)
    -> t n (Entity record)
  dbInsert = lift . dbInsert

  dbInsertUnique
    :: (PersistRecordBackend record SqlBackend)
    => (UTCTime -> record)
    -> m (Maybe (Entity record))
  default dbInsertUnique
    :: ( MonadKucipongDb n
       , MonadTrans t
       , m ~ t n
       , PersistRecordBackend record SqlBackend
       )
    => (UTCTime -> record)
    -> t n (Maybe (Entity record))
  dbInsertUnique = lift . dbInsertUnique

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

  dbUpdate
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> (UTCTime -> [Update record]) -> m ()
  default dbUpdate
    :: ( MonadKucipongDb n
       , MonadTrans t
       , m ~ t n
       , PersistRecordBackend record SqlBackend
       )
    => [Filter record] -> (UTCTime -> [Update record]) -> t n ()
  dbUpdate filters updatesCreator = lift $ dbUpdate filters updatesCreator

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

  -- This can be used if ActionCtxT gets an instance of MonadTransControl.
  -- https://github.com/agrafix/Spock/issues/116
  -- dbRun :: ReaderT SqlBackend m a -> m a
  -- default dbRun
  --   :: forall n t a.
  --      ( MonadKucipongDb n
  --      , MonadTransControl t
  --      , Monad n
  --      , m ~ t n
  --      )
  --   => ReaderT SqlBackend m a -> m a
  -- dbRun readerT =
  --   let liftWithFunc =
  --         liftWith $ \run -> dbRun . ReaderT $ run . runReaderT readerT
  --   in liftWithFunc >>= restoreT . pure

instance MonadKucipongDb m => MonadKucipongDb (ActionCtxT ctx m)
instance MonadKucipongDb m => MonadKucipongDb (ExceptT e m)
instance MonadKucipongDb m => MonadKucipongDb (IdentityT m)
instance MonadKucipongDb m => MonadKucipongDb (KucipongAwsT m)
instance MonadKucipongDb m => MonadKucipongDb (KucipongCookieT m)
instance MonadKucipongDb m => MonadKucipongDb (KucipongSendEmailT m)
instance MonadKucipongDb m => MonadKucipongDb (ReaderT r m)

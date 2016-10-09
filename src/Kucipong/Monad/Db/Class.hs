{-# LANGUAGE DefaultSignatures #-}

module Kucipong.Monad.Db.Class where

import Kucipong.Prelude

import Control.Monad.Trans ( MonadTrans )
import Database.Persist ( Entity )
import Web.Spock ( ActionCtxT )

import Kucipong.Db
        ( Admin, AdminLoginToken, Key
        , Store, StoreLoginToken )
import Kucipong.LoginToken ( LoginToken )
import Kucipong.Monad.Cookie.Trans ( KucipongCookieT )
import Kucipong.Monad.SendEmail.Trans ( KucipongSendEmailT )

-- | Type-class for monads that can perform Db actions.  For instance, querying
-- the database for information or writing new information to the database.
--
-- Default implementations are used to easily derive instances for monads
-- transformers that implement 'MonadTrans'.
class Monad m => MonadKucipongDb m where
    -- TODO
    -- This is rough implementation.
    -- Share functions with admin and store user.

    -- ===========
    --  For Admin
    -- ===========

    dbCreateAdmin
        :: EmailAddress
        -> Text
        -- ^ Admin name
        -> m (Entity Admin)
    default dbCreateAdmin
        :: ( Monad (t n)
           , MonadKucipongDb n
           , MonadTrans t
           , m ~ t n
           )
        => EmailAddress -> Text -> t n (Entity Admin)
    dbCreateAdmin = (lift .) . dbCreateAdmin

    dbCreateAdminMagicLoginToken :: Key Admin -> m (Entity AdminLoginToken)
    default dbCreateAdminMagicLoginToken
        :: ( Monad (t n)
           , MonadKucipongDb n
           , MonadTrans t
           , m ~ t n
           )
        => Key Admin -> t n (Entity AdminLoginToken)
    dbCreateAdminMagicLoginToken = lift . dbCreateAdminMagicLoginToken

    dbFindAdminLoginToken :: LoginToken -> m (Maybe (Entity AdminLoginToken))
    default dbFindAdminLoginToken
        :: ( Monad (t n)
           , MonadKucipongDb n
           , MonadTrans t
           , m ~ t n
           )
        => LoginToken -> t n (Maybe (Entity AdminLoginToken))
    dbFindAdminLoginToken = lift . dbFindAdminLoginToken

    dbUpsertAdmin
        :: EmailAddress
        -> Text
        -- ^ Admin name
        -> m (Entity Admin)
    default dbUpsertAdmin
        :: ( Monad (t n)
           , MonadKucipongDb n
           , MonadTrans t
           , m ~ t n
           )
        => EmailAddress -> Text -> t n (Entity Admin)
    dbUpsertAdmin = (lift .) . dbUpsertAdmin

    -- ===========
    --  For Store
    -- ===========

    dbCreateStore
        :: EmailAddress
        -> Text
        -- ^ Store name
        -> m (Entity Store)
    default dbCreateStore
        :: ( Monad (t n)
           , MonadKucipongDb n
           , MonadTrans t
           , m ~ t n
           )
        => EmailAddress -> Text -> t n (Entity Store)
    dbCreateStore = (lift .) . dbCreateStore

    dbCreateStoreMagicLoginToken :: Key Store -> m (Entity StoreLoginToken)
    default dbCreateStoreMagicLoginToken
        :: ( Monad (t n)
           , MonadKucipongDb n
           , MonadTrans t
           , m ~ t n
           )
        => Key Store -> t n (Entity StoreLoginToken)
    dbCreateStoreMagicLoginToken = lift . dbCreateStoreMagicLoginToken

    dbFindStoreLoginToken :: LoginToken -> m (Maybe (Entity StoreLoginToken))
    default dbFindStoreLoginToken
        :: ( Monad (t n)
           , MonadKucipongDb n
           , MonadTrans t
           , m ~ t n
           )
        => LoginToken -> t n (Maybe (Entity StoreLoginToken))
    dbFindStoreLoginToken = lift . dbFindStoreLoginToken

    dbUpsertStore
        :: EmailAddress
        -> Text
        -- ^ Store name
        -> m (Entity Store)
    default dbUpsertStore
        :: ( Monad (t n)
           , MonadKucipongDb n
           , MonadTrans t
           , m ~ t n
           )
        => EmailAddress -> Text -> t n (Entity Store)
    dbUpsertStore = (lift .) . dbUpsertStore

instance MonadKucipongDb m => MonadKucipongDb (ActionCtxT ctx m)
instance MonadKucipongDb m => MonadKucipongDb (ExceptT e m)
instance MonadKucipongDb m => MonadKucipongDb (IdentityT m)
instance MonadKucipongDb m => MonadKucipongDb (KucipongCookieT m)
instance MonadKucipongDb m => MonadKucipongDb (KucipongSendEmailT m)
instance MonadKucipongDb m => MonadKucipongDb (ReaderT r m)

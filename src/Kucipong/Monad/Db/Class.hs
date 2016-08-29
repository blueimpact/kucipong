{-# LANGUAGE DefaultSignatures #-}

module Kucipong.Monad.Db.Class where

import Kucipong.Prelude

import Control.Monad.Trans ( MonadTrans )
import Database.Persist ( Entity )
import Web.Spock ( ActionCtxT )

import Kucipong.Db ( Admin, AdminLoginToken, Key )

-- | Type-class for monads that can perform Db actions.  For instance, querying
-- the database for information or writing new information to the database.
--
-- Default implementations are used to easily derive instances for monads
-- transformers that implement 'MonadTrans'.
class Monad m => MonadKucipongDb m where
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

instance MonadKucipongDb m => MonadKucipongDb (ExceptT e m)
instance MonadKucipongDb m => MonadKucipongDb (IdentityT m)
instance MonadKucipongDb m => MonadKucipongDb (ReaderT r m)
instance MonadKucipongDb m => MonadKucipongDb (ActionCtxT ctx m)

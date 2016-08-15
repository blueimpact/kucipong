{-# LANGUAGE DefaultSignatures #-}

module Kucipong.Monad.Db.Class where

import Kucipong.Prelude

import Control.Monad.Trans ( MonadTrans )
import Web.Spock ( ActionCtxT )

import Kucipong.Db ( Admin, Key )

-- | Type-class for monads that can perform Db actions.  For instance, querying
-- the database for information or writing new information to the database.
--
-- Default implementations are used to easily derive instances for monads
-- transformers that implement 'MonadTrans'.
class Monad m => MonadKucipongDb m where
    dbInsertNewUser :: EmailAddress -> m (Key Admin)
    default dbInsertNewUser
        :: ( Monad (t n)
           , MonadKucipongDb n
           , MonadTrans t
           , m ~ t n
           )
        => EmailAddress -> t n (Key Admin)
    dbInsertNewUser = lift . dbInsertNewUser

    dbLoginUser :: EmailAddress -> m (Maybe (Key Admin))
    default dbLoginUser
        :: ( Monad (t n)
           , MonadKucipongDb n
           , MonadTrans t
           , m ~ t n
           )
        => EmailAddress -> t n (Maybe (Key Admin))
    dbLoginUser = lift . dbLoginUser

instance MonadKucipongDb m => MonadKucipongDb (ExceptT e m)
instance MonadKucipongDb m => MonadKucipongDb (IdentityT m)
instance MonadKucipongDb m => MonadKucipongDb (ReaderT r m)
instance MonadKucipongDb m => MonadKucipongDb (ActionCtxT ctx m)

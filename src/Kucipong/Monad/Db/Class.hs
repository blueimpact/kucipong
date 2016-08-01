{-# LANGUAGE DefaultSignatures #-}

module Kucipong.Monad.Db.Class where

import Kucipong.Prelude

import Control.Monad.Trans ( MonadTrans )
import Web.Spock ( ActionCtxT )

import Kucipong.Db ( Key, Password, PasswordHash, User )

class Monad m => MonadKucipongDb m where
    dbInsertNewUser :: EmailAddress -> PasswordHash -> m (Key User)
    default dbInsertNewUser
        :: ( Monad (t n)
           , MonadKucipongDb n
           , MonadTrans t
           , m ~ t n
           )
        => EmailAddress -> PasswordHash ->  t n (Key User)
    dbInsertNewUser = (lift .) . dbInsertNewUser

    dbLoginUser :: EmailAddress -> Password -> m (Maybe (Key User))
    default dbLoginUser
        :: ( Monad (t n)
           , MonadKucipongDb n
           , MonadTrans t
           , m ~ t n
           )
        => EmailAddress -> Password ->  t n (Maybe (Key User))
    dbLoginUser = (lift .) . dbLoginUser

instance MonadKucipongDb m => MonadKucipongDb (ExceptT e m)
instance MonadKucipongDb m => MonadKucipongDb (IdentityT m)
instance MonadKucipongDb m => MonadKucipongDb (ReaderT r m)
instance MonadKucipongDb m => MonadKucipongDb (ActionCtxT ctx m)

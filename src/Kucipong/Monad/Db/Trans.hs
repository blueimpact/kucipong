{-# LANGUAGE UndecidableInstances #-}

module Kucipong.Monad.Db.Trans where

import Kucipong.Prelude

import Control.Lens ( to, view )
import Control.Monad.Trans.Class ( MonadTrans )
import Control.Monad.Trans.Control
    ( ComposeSt, MonadBaseControl(..), MonadTransControl(..)
    , defaultLiftBaseWith, defaultRestoreM )
import Crypto.PasswordStore ( verifyPassword )
import Database.Persist ( getBy, insert, entityKey, entityVal )

import Kucipong.Config ( Config )
import Kucipong.Errors ( AppErr )
import Kucipong.Monad.Db.Class ( MonadKucipongDb(..) )

newtype KucipongDbT m a = KucipongDbT { unKucipongDbT :: IdentityT m a }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadBase b
        , MonadError e
        , MonadIO
        , MonadLogger
        , MonadReader r
        , MonadTrans
        )

runKucipongDbT :: KucipongDbT m a -> m a
runKucipongDbT = runIdentityT . unKucipongDbT

instance MonadTransControl KucipongDbT where
    type StT KucipongDbT a = a
    liftWith f = lift (f runKucipongDbT)
    restoreT = KucipongDbT . IdentityT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (MonadBaseControl b m) => MonadBaseControl b (KucipongDbT m) where
    type StM (KucipongDbT m) a = ComposeSt KucipongDbT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

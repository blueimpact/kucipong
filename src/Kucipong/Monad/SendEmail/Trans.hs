{-# LANGUAGE UndecidableInstances #-}

module Kucipong.Monad.SendEmail.Trans where

import Kucipong.Prelude

import Control.Monad.Random ( MonadRandom )
import Control.Monad.Time ( MonadTime )
import Control.Monad.Trans.Class ( MonadTrans )
import Control.Monad.Trans.Control
    ( ComposeSt, MonadBaseControl(..), MonadTransControl(..)
    , defaultLiftBaseWith, defaultRestoreM )


newtype KucipongSendEmailT m a = KucipongSendEmailT { unKucipongSendEmailT :: IdentityT m a }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadBase b
        , MonadCatch
        , MonadError e
        , MonadIO
        , MonadLogger
        , MonadRandom
        , MonadReader r
        , MonadThrow
        , MonadTime
        , MonadTrans
        )

runKucipongSendEmailT :: KucipongSendEmailT m a -> m a
runKucipongSendEmailT = runIdentityT . unKucipongSendEmailT

instance MonadTransControl KucipongSendEmailT where
    type StT KucipongSendEmailT a = a
    liftWith f = lift (f runKucipongSendEmailT)
    restoreT = KucipongSendEmailT . IdentityT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (MonadBaseControl b m) => MonadBaseControl b (KucipongSendEmailT m) where
    type StM (KucipongSendEmailT m) a = ComposeSt KucipongSendEmailT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

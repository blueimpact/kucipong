{-# LANGUAGE UndecidableInstances #-}

module Kucipong.Monad.Cookie.Trans where

import Kucipong.Prelude

import Control.Monad.Random ( MonadRandom )
import Control.Monad.Time ( MonadTime )
import Control.Monad.Trans.Class ( MonadTrans )
import Control.Monad.Trans.Control
    ( ComposeSt, MonadBaseControl(..), MonadTransControl(..)
    , defaultLiftBaseWith, defaultRestoreM )


newtype KucipongCookieT m a = KucipongCookieT { unKucipongCookieT :: IdentityT m a }
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

runKucipongCookieT :: KucipongCookieT m a -> m a
runKucipongCookieT = runIdentityT . unKucipongCookieT

instance MonadTransControl KucipongCookieT where
    type StT KucipongCookieT a = a
    liftWith f = lift (f runKucipongCookieT)
    restoreT = KucipongCookieT . IdentityT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (MonadBaseControl b m) => MonadBaseControl b (KucipongCookieT m) where
    type StM (KucipongCookieT m) a = ComposeSt KucipongCookieT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

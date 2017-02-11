{-# LANGUAGE UndecidableInstances #-}

module Kucipong.Monad.Aws.Trans where

import Kucipong.Prelude

import Control.Monad.Random ( MonadRandom )
import Control.Monad.Time ( MonadTime )
import Control.Monad.Trans.Class ( MonadTrans )
import Control.Monad.Trans.Control
    ( ComposeSt, MonadBaseControl(..), MonadTransControl(..)
    , defaultLiftBaseWith, defaultRestoreM )
import Control.Monad.Trans.Resource (MonadResource(..))

newtype KucipongAwsT m a = KucipongAwsT { unKucipongAwsT :: IdentityT m a }
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

runKucipongAwsT :: KucipongAwsT m a -> m a
runKucipongAwsT = runIdentityT . unKucipongAwsT

instance MonadTransControl KucipongAwsT where
    type StT KucipongAwsT a = a
    liftWith f = lift (f runKucipongAwsT)
    restoreT = KucipongAwsT . IdentityT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (MonadBaseControl b m) => MonadBaseControl b (KucipongAwsT m) where
    type StM (KucipongAwsT m) a = ComposeSt KucipongAwsT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

instance MonadResource m => MonadResource (KucipongAwsT m) where
  liftResourceT = lift . liftResourceT

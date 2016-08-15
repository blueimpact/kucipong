{-# LANGUAGE UndecidableInstances #-}

module Kucipong.Monad
    ( module X
    , module Kucipong.Monad
    ) where

import Kucipong.Prelude

import Control.Monad.Logger ( runStdoutLoggingT )
import Control.Monad.Trans.Class ( MonadTrans )
import Control.Monad.Trans.Control
    ( ComposeSt, MonadBaseControl(..), MonadTransControl(..)
    , defaultLiftBaseWith, defaultRestoreM )

import Kucipong.Config ( Config )
import Kucipong.Errors (AppErr)
import Kucipong.Monad.Db as X

-- | This constraint synonym wraps up all of our Kucipong type classes.
type MonadKucipong' m =
    ( MonadKucipongDb m
    )

-- | This constraint synonym wraps up all of the type classes used by 'KucipongM'.
type MonadKucipong m =
    ( MonadBaseControl IO m
    , MonadError AppErr m
    , MonadIO m
    , MonadLogger m
    , MonadReader Config m
    , MonadKucipong' m
    )

-- | 'KucipongT' is just a wrapper around all of our Monad transformers.
newtype KucipongT m a = KucipongT
    { unKucipongT ::
        KucipongDbT m a
    }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadBase b
        , MonadError e
        , MonadIO
        , MonadLogger
        , MonadReader r
        )

deriving instance
    ( MonadBaseControl IO m
    , MonadError AppErr m
    , MonadIO m
    , MonadReader Config m
    ) => MonadKucipongDb (KucipongT m)


-- | Unwrap the @m@ from 'KucipongT'.
runKucipongT :: KucipongT m a -> m a
runKucipongT =
      runKucipongDbT
    . unKucipongT

-- | Lift an action in @m@ to 'KucipongT'.
liftToKucipongT :: (Monad m) => m a -> KucipongT m a
liftToKucipongT = KucipongT . lift

instance MonadTrans KucipongT where
    lift = liftToKucipongT

instance MonadTransControl KucipongT where
    type StT KucipongT a = a
    liftWith f = lift (f runKucipongT)
    restoreT = liftToKucipongT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance (MonadBaseControl b m) => MonadBaseControl b (KucipongT m) where
    type StM (KucipongT m) a = ComposeSt KucipongT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM
    {-# INLINABLE liftBaseWith #-}
    {-# INLINABLE restoreM #-}

-- | Monad transformer stack for our application.
newtype KucipongM a = KucipongM
    { unKucipongM ::
        KucipongT (ReaderT Config (ExceptT AppErr (LoggingT IO))) a
    }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadBase IO
        , MonadError AppErr
        , MonadIO
        , MonadReader Config
        , MonadLogger
        , MonadKucipongDb
        )

instance MonadBaseControl IO KucipongM where
    type StM KucipongM a = Config -> IO (Either AppErr a)

    liftBaseWith
        :: forall a
         . ((forall x .  KucipongM x -> IO (Config -> IO (Either AppErr x))) -> IO a)
        -> KucipongM a
    liftBaseWith f = liftBase $ f unwrapKucipongM
      where
        unwrapKucipongM
            :: forall z . KucipongM z -> IO (Config -> IO (Either AppErr z))
        unwrapKucipongM kucipongM =
            pure $ \config -> runKucipongM config kucipongM

    restoreM :: forall a . (Config -> IO (Either AppErr a)) -> KucipongM a
    restoreM f = KucipongM $ lift readerT
      where
        readerT :: ReaderT Config (ExceptT AppErr (LoggingT IO)) a
        readerT = ReaderT $ \config -> ExceptT . lift $ f config

-- | Run the 'KucipongM' monad stack.
runKucipongM :: Config -> KucipongM a -> IO (Either AppErr a)
runKucipongM config =
      runStdoutLoggingT
    . runExceptT
    . flip runReaderT config
    . runKucipongT
    . unKucipongM

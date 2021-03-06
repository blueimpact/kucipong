{-# LANGUAGE UndecidableInstances #-}

module Kucipong.Monad
  ( module X
  , module Kucipong.Monad
  ) where

import Kucipong.Prelude

import Control.Monad.Random (MonadRandom)
import Control.Monad.Time (MonadTime)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Control
       (ComposeSt, MonadBaseControl(..), MonadTransControl(..),
        defaultLiftBaseWith, defaultRestoreM)
import Control.Monad.Trans.Resource
       (MonadResource(..), ResourceT, runResourceT)

import Kucipong.Config (Config)
import Kucipong.Errors (AppErr)
import Kucipong.Logger (runLogger)
import Kucipong.Monad.Aws as X
import Kucipong.Monad.Cookie as X
import Kucipong.Monad.Db as X
import Kucipong.Monad.OtherInstances ()
import Kucipong.Monad.SendEmail as X

-- | This constraint synonym wraps up all of our Kucipong type classes.
type MonadKucipong' m =
    ( MonadKucipongAws m
    , MonadKucipongCookie m
    , MonadKucipongDb m
    , MonadKucipongSendEmail m
    )

-- | This constraint synonym wraps up all of the type classes used by 'KucipongM'.
type MonadKucipong m =
    ( MonadBaseControl IO m
    , MonadCatch m
    , MonadError AppErr m
    , MonadIO m
    , MonadLogger m
    , MonadRandom m
    , MonadReader Config m
    , MonadResource m
    , MonadThrow m
    , MonadTime m
    , MonadKucipong' m
    )

-- | 'KucipongT' is just a wrapper around all of our Monad transformers.
newtype KucipongT m a = KucipongT
  { unKucipongT :: KucipongAwsT (KucipongCookieT (KucipongDbT (KucipongSendEmailT m))) a
  } deriving ( Applicative
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
             )

deriving instance
         (MonadCatch m, MonadIO m, MonadReader Config m, MonadResource m) =>
         MonadKucipongAws (KucipongT m)

deriving instance
         (MonadIO m, MonadReader Config m) =>
         MonadKucipongCookie (KucipongT m)

deriving instance
         (MonadBaseControl IO m, MonadCatch m, MonadIO m, MonadRandom m,
          MonadReader Config m, MonadTime m) =>
         MonadKucipongDb (KucipongT m)

deriving instance
         (MonadIO m, MonadReader Config m) =>
         MonadKucipongSendEmail (KucipongT m)

-- | Unwrap the @m@ from 'KucipongT'.
runKucipongT :: KucipongT m a -> m a
runKucipongT =
  runKucipongSendEmailT .
  runKucipongDbT . runKucipongCookieT . runKucipongAwsT . unKucipongT

-- | Lift an action in @m@ to 'KucipongT'.
liftToKucipongT
  :: (Monad m)
  => m a -> KucipongT m a
liftToKucipongT = KucipongT . lift . lift . lift . lift

instance MonadTrans KucipongT where
  lift = liftToKucipongT

instance MonadTransControl KucipongT where
  type StT KucipongT a = a
  liftWith f = lift (f runKucipongT)
  restoreT = liftToKucipongT
  {-# INLINABLE liftWith #-}
  {-# INLINABLE restoreT #-}

instance (MonadBaseControl b m) =>
         MonadBaseControl b (KucipongT m) where
  type StM (KucipongT m) a = ComposeSt KucipongT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance (MonadResource m) =>
         MonadResource (KucipongT m) where
  liftResourceT :: ResourceT IO a -> KucipongT m a
  liftResourceT = liftToKucipongT . liftResourceT

-- | Monad transformer stack for our application.
newtype KucipongM a = KucipongM
  { unKucipongM ::
      KucipongT (ReaderT Config (ExceptT AppErr (LoggingT (ResourceT IO)))) a
  } deriving ( Applicative
             , Functor
             , Monad
             , MonadBase IO
             , MonadCatch
             , MonadError AppErr
             , MonadIO
             , MonadLogger
             , MonadRandom
             , MonadReader Config
             , MonadResource
             , MonadThrow
             , MonadTime
             , MonadKucipongAws
             , MonadKucipongCookie
             , MonadKucipongDb
             , MonadKucipongSendEmail
             )

instance MonadBaseControl IO KucipongM where
  type StM KucipongM a = Config -> IO (Either AppErr a)

  liftBaseWith
    :: forall a.
       ((forall x. KucipongM x -> IO (Config -> IO (Either AppErr x))) -> IO a)
    -> KucipongM a
  liftBaseWith f = liftBase $ f unwrapKucipongM
    where
      unwrapKucipongM
        :: forall z.
           KucipongM z -> IO (Config -> IO (Either AppErr z))
      unwrapKucipongM kucipongM = pure $ \config -> runKucipongM config kucipongM

  restoreM
    :: forall a.
       (Config -> IO (Either AppErr a)) -> KucipongM a
  restoreM f = KucipongM $ lift readerT
    where
      readerT :: ReaderT Config (ExceptT AppErr (LoggingT (ResourceT IO))) a
      readerT = ReaderT $ \config -> ExceptT . lift . lift $ f config

-- | Run the 'KucipongM' monad stack.
runKucipongM :: Config -> KucipongM a -> IO (Either AppErr a)
runKucipongM config =
  runResourceT .
  runLogger .
  runExceptT . flip runReaderT config . runKucipongT . unKucipongM

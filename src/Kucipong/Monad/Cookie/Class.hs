{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Kucipong.Monad.Cookie.Class where

import Kucipong.Prelude

import Control.Monad.Trans (MonadTrans)
import Web.Spock (ActionCtxT, CookieSettings)

import Kucipong.Monad.Aws.Trans (KucipongAwsT)
import Kucipong.Monad.Db.Trans (KucipongDbT)
import Kucipong.Monad.SendEmail.Trans (KucipongSendEmailT)
import Kucipong.Session (Admin, Session, Store)

-- |
-- Default implementations are used to easily derive instances for monads
-- transformers that implement 'MonadTrans'.
class Monad m => MonadKucipongCookie m where
  cookieSettings
    :: m CookieSettings
  default cookieSettings
    :: ( MonadKucipongCookie n
       , MonadTrans t
       , m ~ t n
       )
    => t n CookieSettings
  cookieSettings = lift cookieSettings

  encryptSessionCookie
    :: Session sessionType -> m Text
  default encryptSessionCookie
    :: ( MonadKucipongCookie n
       , MonadTrans t
       , m ~ t n
       )
    => Session sessionType -> t n Text
  encryptSessionCookie = lift . encryptSessionCookie

  decryptAdminSessionCookie
    :: Text -> m (Maybe (Session Admin))
  default decryptAdminSessionCookie
    :: ( MonadKucipongCookie n
       , MonadTrans t
       , m ~ t n
       )
    => Text -> t n (Maybe (Session Admin))
  decryptAdminSessionCookie  = lift . decryptAdminSessionCookie

  decryptStoreSessionCookie
    :: Text -> m (Maybe (Session Store))
  default decryptStoreSessionCookie
    :: ( MonadKucipongCookie n
       , MonadTrans t
       , m ~ t n
       )
    => Text -> t n (Maybe (Session Store))
  decryptStoreSessionCookie  = lift . decryptStoreSessionCookie

instance MonadKucipongCookie m => MonadKucipongCookie (ActionCtxT ctx m)
instance MonadKucipongCookie m => MonadKucipongCookie (ExceptT e m)
instance MonadKucipongCookie m => MonadKucipongCookie (IdentityT m)
instance MonadKucipongCookie m => MonadKucipongCookie (KucipongAwsT m)
instance MonadKucipongCookie m => MonadKucipongCookie (KucipongDbT m)
instance MonadKucipongCookie m => MonadKucipongCookie (KucipongSendEmailT m)
instance MonadKucipongCookie m => MonadKucipongCookie (ReaderT r m)

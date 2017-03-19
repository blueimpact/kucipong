{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.Cookie.Instance where

import Kucipong.Prelude

import Web.Spock
    ( CookieSettings(..), CookieEOL(..), defaultCookieSettings )

import Kucipong.Environment
       (Environment(Production), HasEnv(getEnv))
import Kucipong.Monad.Cookie.Class (MonadKucipongCookie(..))
import Kucipong.Monad.Cookie.Trans (KucipongCookieT(..))
import Kucipong.Session
       (Admin, HasSessionKey, Session, Store, decryptAdminSession,
        decryptStoreSession, encryptSession)
import Kucipong.Util (oneYear)

instance
  ( HasEnv r
  , HasSessionKey r
  , MonadIO m
  , MonadReader r m
  ) => MonadKucipongCookie (KucipongCookieT m) where

  cookieSettings :: KucipongCookieT m CookieSettings
  cookieSettings = do
    env <- reader getEnv
    case env of
      Production -> pure develCookieSettings { cs_secure = True }
      _ -> pure develCookieSettings
    where
      develCookieSettings :: CookieSettings
      develCookieSettings = defaultCookieSettings
        { cs_EOL = CookieValidFor oneYear
        , cs_HTTPOnly = True
        , cs_secure = False
        , cs_domain = Nothing
        , cs_path = Just "/"
        }

  encryptSessionCookie :: Session sessionType -> KucipongCookieT m Text
  encryptSessionCookie = lift . encryptSession

  decryptAdminSessionCookie
    :: Text -> KucipongCookieT m (Maybe (Session Admin))
  decryptAdminSessionCookie = decryptAdminSession

  decryptStoreSessionCookie
    :: Text -> KucipongCookieT m (Maybe (Session Store))
  decryptStoreSessionCookie = decryptStoreSession

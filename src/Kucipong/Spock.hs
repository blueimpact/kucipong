
module Kucipong.Spock where

import Kucipong.Prelude

import Web.Spock ( ActionCtxT, setCookie )

import Kucipong.Monad ( MonadKucipongCookie(cookieSettings, encryptSessionCookie) )
import Kucipong.Session ( Session(AdminSession, StoreSession) )

setAdminCookie
    :: ( MonadIO m
       , MonadKucipongCookie m
       )
    => EmailAddress -> ActionCtxT ctx m ()
setAdminCookie = setCookieGeneric "adminEmail" . AdminSession

setStoreCookie
    :: ( MonadIO m
       , MonadKucipongCookie m
       )
    => EmailAddress -> ActionCtxT ctx m ()
setStoreCookie = setCookieGeneric "storeEmail" . StoreSession

setCookieGeneric
    :: ( MonadIO m
       , MonadKucipongCookie m
       )
    => Text -> Session sessionType -> ActionCtxT ctx m ()
setCookieGeneric cookieKey sessionVal = do
    cookieVal <- encryptSessionCookie sessionVal
    settings <- cookieSettings
    setCookie cookieKey cookieVal settings

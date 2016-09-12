
module Kucipong.Spock
    ( setAdminCookie
    , setStoreCookie
    , getAdminCookie
    , getStoreCookie
    ) where

import Kucipong.Prelude

import Web.Spock ( ActionCtxT, cookie, setCookie )

import Kucipong.Monad ( MonadKucipongCookie(..) )
import Kucipong.Session ( Admin, Session(AdminSession, StoreSession), Store )

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

getAdminCookie
    :: ( MonadIO m
       , MonadKucipongCookie m
       )
    => ActionCtxT ctx m (Maybe (Session Admin))
getAdminCookie = getCookieGeneic "adminEmail" decryptAdminSessionCookie

getStoreCookie
    :: ( MonadIO m
       , MonadKucipongCookie m
       )
    => ActionCtxT ctx m (Maybe (Session Store))
getStoreCookie = getCookieGeneic "storeEmail" decryptStoreSessionCookie

getCookieGeneic
    :: ( MonadIO m
       , MonadKucipongCookie m
       )
    => Text
    -> (Text -> ActionCtxT ctx m (Maybe (Session sessionType)))
    -> ActionCtxT ctx m (Maybe (Session sessionType))
getCookieGeneic cookieKey cookieValDecryptFun = do
    maybeRawCookie <- cookie cookieKey
    maybe (pure Nothing) cookieValDecryptFun maybeRawCookie


module Kucipong.Spock
    ( module Kucipong.Spock.ReqParam
    , setAdminCookie
    , setStoreCookie
    , getAdminCookie
    , getStoreCookie
    , ContainsAdminSession
    , getAdminEmail
    , ContainsStoreSession
    , getStoreEmail
    ) where

import Kucipong.Prelude

import Data.HVect ( HVect, ListContains, findFirst )
import Web.Spock ( ActionCtxT, cookie, getContext, setCookie )

import Kucipong.Monad ( MonadKucipongCookie(..) )
import Kucipong.Session ( Admin, Session(AdminSession, StoreSession), Store )
import Kucipong.Spock.ReqParam

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

getAdminEmail
    :: forall n xs m
     . ( ContainsAdminSession n xs
       , MonadIO m
       )
    => ActionCtxT (HVect xs) m (Session Admin)
getAdminEmail = findFirst <$> getContext

type ContainsAdminSession n xs = ListContains n (Session Admin) xs

getStoreEmail
    :: forall n xs m
     . ( ContainsStoreSession n xs
       , MonadIO m
       )
    => ActionCtxT (HVect xs) m (Session Store)
getStoreEmail = findFirst <$> getContext

type ContainsStoreSession n xs = ListContains n (Session Store) xs

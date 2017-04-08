
module Kucipong.Spock
    ( module Kucipong.Spock.Json
    , module Kucipong.Spock.ReqParam
    , setAdminCookie
    , setStoreCookie
    , getAdminCookie
    , getStoreCookie
    , ContainsAdminSession
    , getAdminEmail
    , ContainsStoreSession
    , getStoreKey
    , pattern StoreSession
    ) where

import Kucipong.Prelude

import Data.HVect (HVect, ListContains, findFirst)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Web.Spock (ActionCtxT, cookie, getContext, setCookie)

import Kucipong.Db (Key)
import qualified Kucipong.Db as Db
import Kucipong.Monad (MonadKucipongCookie(..))
import Kucipong.Session
       (Session(AdminSession, StoreSessionRaw),
        SessionType(SessionTypeAdmin, SessionTypeStore))
import Kucipong.Spock.Json
import Kucipong.Spock.ReqParam

setAdminCookie
  :: (MonadIO m, MonadKucipongCookie m)
  => EmailAddress -> ActionCtxT ctx m ()
setAdminCookie = setCookieGeneric "adminEmail" . AdminSession

setStoreCookie
  :: (MonadIO m, MonadKucipongCookie m)
  => Key Db.Store -> ActionCtxT ctx m ()
setStoreCookie = setCookieGeneric "storeEmail" . StoreSessionRaw . fromSqlKey

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
    => ActionCtxT ctx m (Maybe (Session 'SessionTypeAdmin))
getAdminCookie = getCookieGeneic "adminEmail" decryptAdminSessionCookie

getStoreCookie
    :: ( MonadIO m
       , MonadKucipongCookie m
       )
    => ActionCtxT ctx m (Maybe (Session 'SessionTypeStore))
getStoreCookie = getCookieGeneic "storeEmail" decryptStoreSessionCookie

getCookieGeneic
    :: MonadIO m
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
    => ActionCtxT (HVect xs) m (Session 'SessionTypeAdmin)
getAdminEmail = findFirst <$> getContext

type ContainsAdminSession n xs = ListContains n (Session 'SessionTypeAdmin) xs

getStoreKey
    :: forall n xs m
     . ( ContainsStoreSession n xs
       , MonadIO m
       )
    => ActionCtxT (HVect xs) m (Session 'SessionTypeStore)
getStoreKey = findFirst <$> getContext

type ContainsStoreSession n xs = ListContains n (Session 'SessionTypeStore) xs

pattern StoreSession :: Key Db.Store -> Session 'SessionTypeStore
pattern StoreSession storeKey <- StoreSessionRaw (toSqlKey -> storeKey)

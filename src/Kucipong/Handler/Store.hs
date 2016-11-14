{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store where

import Kucipong.Prelude

import Control.FromSum (fromMaybeM)
import Control.Monad.Time (MonadTime(..))
import Data.Aeson ((.=))
import Data.HVect (HVect(..))
import Database.Persist (Entity(..))
import Text.EDE (fromPairs)
import Web.Routing.Combinators (PathState(Open))
import Web.Spock
       (ActionCtxT, Path, (<//>), getContext, prehook, root,
        redirect, renderRoute, var)
import Web.Spock.Core (SpockCtxT, get, post)

import Kucipong.Db
       (Key(..), LoginTokenExpirationTime(..), Store(..),
        StoreLoginToken(storeLoginTokenExpirationTime,
                        storeLoginTokenLoginToken))
import Kucipong.Email (EmailError)
import Kucipong.Form
       (StoreEditForm(..), StoreLoginForm(StoreLoginForm))
import Kucipong.LoginToken (LoginToken)
import Kucipong.Monad
       (MonadKucipongCookie, MonadKucipongDb(..),
        MonadKucipongSendEmail(..), dbFindStoreByEmail,
        dbFindStoreLoginToken, dbUpsertStore)
import Kucipong.RenderTemplate (renderTemplateFromEnv)
import Kucipong.Session (Store, Session(..))
import Kucipong.Spock
       (ContainsStoreSession, getReqParamErr, getStoreCookie,
        getStoreEmail, setStoreCookie)

-- | Url prefix for all of the following 'Path's.
storeUrlPrefix :: Path '[] 'Open
storeUrlPrefix = "store"

rootR :: Path '[] 'Open
rootR = ""

loginR :: Path '[] 'Open
loginR = "login"

doLoginR :: Path '[LoginToken] 'Open
doLoginR = loginR <//> var

editR :: Path '[] 'Open
editR = "edit"

-- | Handler for returning the store login page.
loginGet
  :: forall ctx m.
     (MonadIO m)
  => ActionCtxT ctx m ()
loginGet = $(renderTemplateFromEnv "storeUser_login.html") $ fromPairs []

-- | Handler for sending an email to the store owner that they can use to
-- login.
loginPost
  :: forall xs m.
     (MonadIO m, MonadKucipongDb m, MonadKucipongSendEmail m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
loginPost = do
  (StoreLoginForm email) <- getReqParamErr handleErr
  maybeStoreEntity <- dbFindStoreByEmail email
  (Entity (StoreKey storeEmailKey) _) <-
    fromMaybeM
      (handleErr $ "Could not find store for email " <> tshow email)
      maybeStoreEntity
  (Entity _ storeLoginToken) <- dbCreateStoreMagicLoginToken storeEmailKey
  maybe (pure ()) handleSendEmailFail =<<
    sendStoreLoginEmail email (storeLoginTokenLoginToken storeLoginToken)
  $(renderTemplateFromEnv "storeUser_login.html") $
    fromPairs
      ["messages" .= ["We have sent you an email with verification URL." :: Text]]
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $ "got following error in store loginPost handler: " <> errMsg
      $(renderTemplateFromEnv "storeUser_login.html") $
        fromPairs ["errors" .= [errMsg]]

    handleSendEmailFail :: EmailError -> ActionCtxT (HVect xs) m a
    handleSendEmailFail emailError = do
      $(logDebug) $ "got email error in store loginPost: " <> tshow emailError
      handleErr "could not send email"

-- | Login an store.  Take the store's 'LoginToken', and send them a session
-- cookie.
doLogin
  :: forall ctx m.
     (MonadIO m, MonadKucipongCookie m, MonadKucipongDb m, MonadTime m)
  => LoginToken -> ActionCtxT ctx m ()
doLogin loginToken = do
  maybeStoreLoginTokenEntity <- dbFindStoreLoginToken loginToken
  (Entity (StoreLoginTokenKey (StoreEmailKey storeEmail)) storeLoginToken) <-
    fromMaybeM noStoreLoginTokenError maybeStoreLoginTokenEntity
  -- check date on store login token
  now <- currentTime
  let (LoginTokenExpirationTime expirationTime) =
        storeLoginTokenExpirationTime storeLoginToken
  when (now > expirationTime) tokenExpiredError
  setStoreCookie storeEmail
  redirect $ renderRoute root
  where
    noStoreLoginTokenError :: ActionCtxT ctx m a
    noStoreLoginTokenError =
      redirect . renderRoute $ storeUrlPrefix <//> loginR
    tokenExpiredError :: ActionCtxT ctx m a
    tokenExpiredError = redirect . renderRoute $ storeUrlPrefix <//> loginR

storeGet
  :: forall xs n m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m)
  => ActionCtxT (HVect xs) m ()
storeGet = do
  (StoreSession email) <- getStoreEmail
  maybeStore <- fmap entityVal <$> dbFindStoreByEmail email
  Store { storeName
        , storeSalesPoint
        , storeBusinessCategory
        , storeBusinessCategoryDetail
        , storeAddress
        , storePhoneNumber
        , storeBusinessHours
        , storeRegularHoliday
        , storeUrl
        } <- fromMaybeM handleNoStoreError maybeStore
  $(renderTemplateFromEnv "storeUser_store.html") $
    fromPairs
      [ "name" .= storeName
      , "businessCategory" .= storeBusinessCategory
      , "businessCategoryDetail" .= storeBusinessCategoryDetail
      , "salesPoint" .= storeSalesPoint
      , "address" .= storeAddress
      , "phoneNumber" .= storePhoneNumber
      , "businessHours" .= storeBusinessHours
      , "regularHoliday" .= storeRegularHoliday
      , "url" .= storeUrl
      ]
  where
    handleNoStoreError :: ActionCtxT (HVect xs) m a
    handleNoStoreError =
      redirect . renderRoute $ storeUrlPrefix <//> editR

storeEditGet
  :: forall xs n m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m)
  => ActionCtxT (HVect xs) m ()
storeEditGet = do
  (StoreSession email) <- getStoreEmail
  maybeStore <- fmap entityVal <$> dbFindStoreByEmail email
  $(renderTemplateFromEnv "storeUser_store_edit.html") $
    fromPairs
      [ "name" .= (storeName <$> maybeStore)
      , "businessCategory" .= (storeBusinessCategory <$> maybeStore)
      , "businessCategoryDetail" .= (storeBusinessCategoryDetail <$> maybeStore)
      , "salesPoint" .= (maybeStore >>= storeSalesPoint)
      , "address" .= (maybeStore >>= storeAddress)
      , "phoneNumber" .= (maybeStore >>= storePhoneNumber)
      , "businessHours" .= (maybeStore >>= storeBusinessHours)
      , "regularHoliday" .= (maybeStore >>= storeRegularHoliday)
      , "url" .= (maybeStore >>= storeUrl)
      ]

storeEditPost
  :: forall xs n m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
storeEditPost = do
  (StoreSession email) <- getStoreEmail
  StoreEditForm { name
                , businessCategory
                , salesPoint
                , address
                , phoneNumber
                , businessHours
                , regularHoliday
                , url
                } <- getReqParamErr handleErr
  void $
    dbUpsertStore
      email
      name
      businessCategory
      ""
      Nothing
      salesPoint
      address
      phoneNumber
      businessHours
      regularHoliday
      url
  redirect . renderRoute $ storeUrlPrefix
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $ "got following error in storeEditPost handler: " <> errMsg
      $(renderTemplateFromEnv "storeUser_store_edit.html") $
        fromPairs ["errors" .= [errMsg]]

storeAuthHook
  :: (MonadIO m, MonadKucipongCookie m)
  => ActionCtxT (HVect xs) m (HVect ((Session Kucipong.Session.Store) ': xs))
storeAuthHook = do
  maybeStoreSession <- getStoreCookie
  case maybeStoreSession of
    Nothing -> redirect . renderRoute $ storeUrlPrefix <//> loginR
    Just storeSession -> do
      oldCtx <- getContext
      return $ storeSession :&: oldCtx

storeComponent
  :: forall m xs.
     ( MonadIO m
     , MonadKucipongCookie m
     , MonadKucipongDb m
     , MonadKucipongSendEmail m
     , MonadLogger m
     , MonadTime m
     )
  => SpockCtxT (HVect xs) m ()
storeComponent = do
  get doLoginR doLogin
  get loginR loginGet
  post loginR loginPost
  prehook storeAuthHook $ do
    get rootR storeGet
    get editR storeEditGet
    post editR storeEditPost

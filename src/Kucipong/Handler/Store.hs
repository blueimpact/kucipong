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
       (ActionCtxT, Path, (<//>), getContext, root, redirect, renderRoute,
        var)
import Web.Spock.Core (SpockCtxT, get, post)

import Kucipong.Db
       (Key(..), LoginTokenExpirationTime(..),
        StoreLoginToken(storeLoginTokenExpirationTime,
                        storeLoginTokenLoginToken))
import Kucipong.Email (EmailError)
import Kucipong.Form (StoreLoginForm(StoreLoginForm))
import Kucipong.LoginToken (LoginToken)
import Kucipong.Monad
       (MonadKucipongCookie, MonadKucipongDb(..),
        MonadKucipongSendEmail(..), dbFindStoreByEmail, dbFindStoreLoginToken)
import Kucipong.RenderTemplate (renderTemplateFromEnv)
import Kucipong.Session (Store, Session(..))
import Kucipong.Spock (getReqParamErr, getStoreCookie, setStoreCookie)

-- | Url prefix for all of the following 'Path's.
storeUrlPrefix :: Path '[] 'Open
storeUrlPrefix = "store"

rootR :: Path '[] 'Open
rootR = ""

loginR :: Path '[] 'Open
loginR = "login"

doLoginR :: Path '[LoginToken] 'Open
doLoginR = loginR <//> var

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
    -- prehook storeAuthHook $
    --     get ("store" <//> "something") someAction

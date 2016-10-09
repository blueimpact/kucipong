{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store where

import Kucipong.Prelude

import Control.Lens ( (^.) )
import Control.Monad.Time ( MonadTime(..) )
import Data.Aeson ( (.=) )
import Data.HVect ( HVect(..) )
import Database.Persist ( Entity(..) )
import Network.HTTP.Types ( forbidden403 )
import Text.EDE ( eitherParse, eitherRender, fromPairs )
import Web.Spock
    ( ActionCtxT, Path, SpockCtxT, (<//>), get, getContext, html, prehook, root
    , redirect, renderRoute, runSpock, setStatus, spockT, text, var )

import Kucipong.Db
    ( Store, StoreId, StoreLoginToken, Key(..), LoginTokenExpirationTime(..)
    , storeLoginTokenExpirationTime )
import Kucipong.LoginToken ( LoginToken )
import Kucipong.Monad
    ( MonadKucipongCookie, MonadKucipongDb(..), MonadKucipongSendEmail )
import Kucipong.RenderTemplate ( renderTemplateFromEnv )
import Kucipong.Spock
    ( ContainsStoreSession, getStoreCookie, getStoreEmail, setStoreCookie )
import Kucipong.Session ( Store, Session(..) )
import Kucipong.Util ( fromEitherM, fromMaybeM )

-- | Url prefix for all of the following 'Path's.
storeUrlPrefix :: Path '[]
storeUrlPrefix = "store"

loginPageR :: Path '[]
loginPageR = "login"

doLoginR :: Path '[LoginToken]
doLoginR = loginPageR <//> var

loginPage
    :: forall ctx m
     . ( MonadIO m
       )
    => ActionCtxT ctx m ()
loginPage =
    $(renderTemplateFromEnv "storeUser_login.html") $ fromPairs []

-- | Login an store.  Take the store's 'LoginToken', and send them a session
-- cookie.
doLogin
    :: forall ctx m
     . ( MonadIO m
       , MonadKucipongCookie m
       , MonadKucipongDb m
       , MonadTime m
       )
    => LoginToken -> ActionCtxT ctx m ()
doLogin loginToken = do
    maybeStoreLoginTokenEntity <- dbFindStoreLoginToken loginToken
    (Entity (StoreLoginTokenKey (StoreEmailKey storeEmail)) storeLoginToken) <-
        fromMaybeM noStoreLoginTokenError maybeStoreLoginTokenEntity
    -- check date on store login token
    now <- currentTime
    let (LoginTokenExpirationTime expirationTime) =
            storeLoginToken ^. storeLoginTokenExpirationTime
    when (now > expirationTime) tokenExpiredError
    setStoreCookie storeEmail
    redirect $ renderRoute root
  where
    noStoreLoginTokenError :: ActionCtxT ctx m a
    noStoreLoginTokenError =
        redirect . renderRoute $ storeUrlPrefix <//> loginPageR

    tokenExpiredError :: ActionCtxT ctx m a
    tokenExpiredError =
        redirect . renderRoute $ storeUrlPrefix <//> loginPageR

storeAuthHook
    :: ( MonadIO m
       , MonadKucipongCookie m
       )
    => ActionCtxT (HVect xs) m (HVect ((Session Kucipong.Session.Store) ': xs))
storeAuthHook = do
    maybeStoreSession <- getStoreCookie
    case maybeStoreSession of
        Nothing ->
            redirect . renderRoute $ storeUrlPrefix <//> loginPageR
        Just storeSession -> do
            oldCtx <- getContext
            return $ storeSession :&: oldCtx

-- TODO: It is better to make some module to share functions with admin handler.
storeComponent
    :: forall m xs
     . ( MonadIO m
       , MonadKucipongCookie m
       , MonadKucipongDb m
       , MonadKucipongSendEmail m
       , MonadLogger m
       , MonadTime m
       )
    => SpockCtxT (HVect xs) m ()
storeComponent = do
    get doLoginR doLogin
    get loginPageR loginPage
    -- prehook storeAuthHook $
    --     get ("store" <//> "something") someAction

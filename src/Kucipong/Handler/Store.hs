{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store where

import Kucipong.Prelude

import Control.FromSum ( fromEitherM, fromMaybeM )
import Control.Lens ( (^.) )
import Control.Monad.Time ( MonadTime(..) )
import Data.Aeson ( (.=) )
import Data.HVect ( HVect(..) )
import Database.Persist ( Entity(..) )
import Network.HTTP.Types ( forbidden403 )
import Text.EDE ( eitherParse, eitherRender, fromPairs )
import Web.Routing.Combinators ( PathState(Open) )
import Web.Spock
    ( ActionCtxT, Path, (<//>), getContext, html
    , root, redirect, renderRoute, runSpock, setStatus, text, var )
import Web.Spock.Core ( SpockCtxT, spockT, get, post, prehook )

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

-- | Url prefix for all of the following 'Path's.
storeUrlPrefix :: Path '[] 'Open
storeUrlPrefix = "store"

loginPageR :: Path '[] 'Open
loginPageR = "login"

doLoginR :: Path '[LoginToken] 'Open
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
            storeLoginTokenExpirationTime storeLoginToken
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

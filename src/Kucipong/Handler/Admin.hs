{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Admin where

import Kucipong.Prelude

import Control.Lens ( (^.) )
import Control.Monad.Time ( MonadTime(..) )
import Data.Aeson ( (.=) )
import Data.HVect ( HVect(..) )
import Database.Persist ( Entity(..) )
import Network.HTTP.Types ( forbidden403 )
import Text.EDE ( eitherParse, eitherRender, fromPairs )
import Web.Spock
    ( ActionCtxT, Path, SpockCtxT, (<//>), get, getContext, html, post, prehook
    , root, redirect, renderRoute, runSpock, setStatus, spockT, text, var )

import Kucipong.Db
    ( Admin, AdminId, AdminLoginToken, Key(..), LoginTokenExpirationTime(..)
    , adminLoginTokenExpirationTime )
import Kucipong.Form ( AdminStoreCreateForm )
import Kucipong.LoginToken ( LoginToken )
import Kucipong.Monad
    ( MonadKucipongCookie, MonadKucipongDb(..), MonadKucipongSendEmail )
import Kucipong.RenderTemplate ( renderTemplateFromEnv )
import Kucipong.Spock
    ( ContainsAdminSession, getAdminCookie, getAdminEmail, getReqParam
    , setAdminCookie )
import Kucipong.Session ( Admin, Session(..) )
import Kucipong.Util ( fromEitherM, fromMaybeM )

-- | Url prefix for all of the following 'Path's.
adminUrlPrefix :: Path '[]
adminUrlPrefix = "admin"

loginR :: Path '[]
loginR = "login"

doLoginR :: Path '[LoginToken]
doLoginR = loginR <//> var

storeCreateR :: Path '[]
storeCreateR = "store" <//> "create"

loginGet
    :: forall ctx m
     . ( MonadIO m
       )
    => ActionCtxT ctx m ()
loginGet =
    $(renderTemplateFromEnv "adminUser_login.html") $ fromPairs []

-- | Login an admin.  Take the admin's 'LoginToken', and send them a session
-- cookie.
doLoginGet
    :: forall ctx m
     . ( MonadIO m
       , MonadKucipongCookie m
       , MonadKucipongDb m
       , MonadTime m
       )
    => LoginToken -> ActionCtxT ctx m ()
doLoginGet loginToken = do
    maybeAdminLoginTokenEntity <- dbFindAdminLoginToken loginToken
    (Entity (AdminLoginTokenKey (AdminKey adminEmail)) adminLoginToken) <-
        fromMaybeM noAdminLoginTokenError maybeAdminLoginTokenEntity
    -- check date on admin login token
    now <- currentTime
    let (LoginTokenExpirationTime expirationTime) =
            adminLoginToken ^. adminLoginTokenExpirationTime
    when (now > expirationTime) tokenExpiredError
    setAdminCookie adminEmail
    redirect $ renderRoute root
  where
    noAdminLoginTokenError :: ActionCtxT ctx m a
    noAdminLoginTokenError =
        redirect . renderRoute $ adminUrlPrefix <//> loginR

    tokenExpiredError :: ActionCtxT ctx m a
    tokenExpiredError =
        redirect . renderRoute $ adminUrlPrefix <//> loginR

-- | Return the store create page for an admin.
storeCreateGet
    :: forall xs n m
     . ( ContainsAdminSession n xs
       , MonadIO m
       , MonadLogger m
       )
    => ActionCtxT (HVect xs) m ()
storeCreateGet = do
    (AdminSession email) <- getAdminEmail
    $(renderTemplateFromEnv "adminUser_admin_store_create.html") $ fromPairs
        [ "adminEmail" .= email ]

storeCreatePost
    :: forall xs n m
     . ( ContainsAdminSession n xs
       , MonadIO m
       , MonadLogger m
       )
    => ActionCtxT (HVect xs) m ()
storeCreatePost = do
    (AdminSession email) <- getAdminEmail
    (storeCreateForm :: AdminStoreCreateForm) <- getReqParam
    redirect $ renderRoute root

adminAuthHook
    :: ( MonadIO m
       , MonadKucipongCookie m
       )
    => ActionCtxT (HVect xs) m (HVect ((Session Kucipong.Session.Admin) ': xs))
adminAuthHook = do
    maybeAdminSession <- getAdminCookie
    case maybeAdminSession of
        Nothing ->
            redirect . renderRoute $ adminUrlPrefix <//> loginR
        Just adminSession -> do
            oldCtx <- getContext
            return $ adminSession :&: oldCtx

adminComponent
    :: forall m xs
     . ( MonadIO m
       , MonadKucipongCookie m
       , MonadKucipongDb m
       , MonadKucipongSendEmail m
       , MonadLogger m
       , MonadTime m
       )
    => SpockCtxT (HVect xs) m ()
adminComponent = do
    get doLoginR doLoginGet
    get loginR loginGet
    prehook adminAuthHook $ do
        get storeCreateR storeCreateGet
        post storeCreateR storeCreatePost


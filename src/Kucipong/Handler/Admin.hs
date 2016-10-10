{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Admin where

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
    ( Admin, AdminId, AdminLoginToken, Key(..), LoginTokenExpirationTime(..)
    , adminLoginTokenExpirationTime, storeEmailEmail
    , storeLoginTokenLoginToken )
import Kucipong.Form ( AdminStoreCreateForm(AdminStoreCreateForm) )
import Kucipong.LoginToken ( LoginToken )
import Kucipong.Monad
    ( MonadKucipongCookie, MonadKucipongDb(..), MonadKucipongSendEmail(..) )
import Kucipong.RenderTemplate ( renderTemplateFromEnv )
import Kucipong.Spock
    ( ContainsAdminSession, getAdminCookie, getAdminEmail, getReqParam
    , setAdminCookie )
import Kucipong.Session ( Admin, Session(..) )

-- | Url prefix for all of the following 'Path's.
adminUrlPrefix :: Path '[] 'Open
adminUrlPrefix = "admin"

loginR :: Path '[] 'Open
loginR = "login"

doLoginR :: Path '[LoginToken] 'Open
doLoginR = loginR <//> var

storeCreateR :: Path '[] 'Open
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
            adminLoginTokenExpirationTime adminLoginToken
    when (now > expirationTime) tokenExpiredError
    setAdminCookie adminEmail
    redirect $ renderRoute root
  where
    noAdminLoginTokenError :: ActionCtxT ctx m a
    noAdminLoginTokenError = do
        setStatus forbidden403
        $(renderTemplateFromEnv "adminUser_login.html") $ fromPairs
            [ "errors" .=
              [ "Failed to log in X(\nPlease try again." :: Text ]
            ]

    tokenExpiredError :: ActionCtxT ctx m a
    tokenExpiredError = do
        setStatus forbidden403
        $(renderTemplateFromEnv "adminUser_login.html") $ fromPairs
            [ "errors" .=
              [ "This log in URL has been expired X(\nPlease try again." :: Text ]
            ]

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
       , MonadKucipongDb m
       , MonadKucipongSendEmail m
       , MonadLogger m
       )
    => ActionCtxT (HVect xs) m ()
storeCreatePost = do
    (AdminSession email) <- getAdminEmail
    (AdminStoreCreateForm storeEmailParam) <- getReqParam
    (Entity storeEmailKey storeEmail) <- dbCreateStoreEmail storeEmailParam
    (Entity storeLoginTokenKey storeLoginToken) <-
        dbCreateStoreMagicLoginToken storeEmailKey
    sendStoreLoginEmail
        (storeEmailEmail storeEmail)
        (storeLoginTokenLoginToken storeLoginToken)
    redirect . renderRoute $ adminUrlPrefix <//> storeCreateR

adminAuthHook
    :: ( MonadIO m
       , MonadKucipongCookie m
       )
    => ActionCtxT (HVect xs) m (HVect ((Session Kucipong.Session.Admin) ': xs))
adminAuthHook = do
    maybeAdminSession <- getAdminCookie
    case maybeAdminSession of
        Nothing ->
          $(renderTemplateFromEnv "adminUser_login.html") $ fromPairs
              [ "errors" .=
                [ "Need to be logged in as admin in order to access this page." :: Text ]
              ]
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


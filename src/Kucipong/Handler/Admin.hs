{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Admin where

import Kucipong.Prelude

import Control.FromSum (fromEitherM, fromMaybeM)
import Control.Monad.Time (MonadTime(..))
import Data.Aeson ((.=))
import Data.HVect (HVect(..))
import Database.Persist (Entity(..))
import Network.HTTP.Types (forbidden403)
import Text.EDE (fromPairs)
import Web.Routing.Combinators (PathState(Open))
import Web.Spock
       (ActionCtxT, Path, (<//>), getContext, redirect, renderRoute,
        setStatus, var)
import Web.Spock.Core (SpockCtxT, get, post, prehook)

import Kucipong.Db
       (DbSafeError(..), Key(..), LoginTokenExpirationTime(..),
        adminLoginTokenExpirationTime, adminLoginTokenLoginToken,
        storeEmailEmail, storeLoginTokenLoginToken)
import Kucipong.Form (AdminLoginForm(..), AdminStoreCreateForm(..))
import Kucipong.LoginToken (LoginToken)
import Kucipong.Monad
       (MonadKucipongCookie, MonadKucipongDb(..),
        MonadKucipongSendEmail(..))
import Kucipong.RenderTemplate (renderTemplateFromEnv)
import Kucipong.Session (Admin, Session(..))
import Kucipong.Spock
       (ContainsAdminSession, getAdminCookie, getAdminEmail,
        getReqParamErr, setAdminCookie)

-- | Url prefix for all of the following 'Path's.
adminUrlPrefix :: Path '[] 'Open
adminUrlPrefix = "admin"

rootR :: Path '[] 'Open
rootR = ""

loginR :: Path '[] 'Open
loginR = "login"

doLoginR :: Path '[LoginToken] 'Open
doLoginR = loginR <//> var

storeCreateR :: Path '[] 'Open
storeCreateR = "store" <//> "create"

-- | Handler for returning the admin login page.
loginGet
  :: forall ctx m.
     (MonadIO m)
  => ActionCtxT ctx m ()
loginGet = $(renderTemplateFromEnv "adminUser_login.html") mempty

-- | Handler for sending an email to the admin that they can use to login.
loginPost
  :: forall xs m.
     (MonadIO m, MonadKucipongDb m, MonadKucipongSendEmail m)
  => ActionCtxT (HVect xs) m ()
loginPost = do
  (AdminLoginForm email) <- getReqParamErr handleErr
  maybeAdminEntity <- dbFindAdmin email
  (Entity adminKey _) <-
    fromMaybeM (handleErr "Could not login.") maybeAdminEntity
  (Entity _ adminLoginToken) <- dbCreateAdminMagicLoginToken adminKey
  sendAdminLoginEmail email (adminLoginTokenLoginToken adminLoginToken)
  $(renderTemplateFromEnv "adminUser_login.html") $
    fromPairs
      ["messages" .= ["We have sent you email with verification URL." :: Text]]
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg =
      $(renderTemplateFromEnv "adminUser_login.html") $
      fromPairs ["errors" .= [errMsg]]

-- | Login an admin.  Take the admin's 'LoginToken', and send them a session
-- cookie.
doLoginGet
  :: forall ctx m.
     (MonadIO m, MonadKucipongCookie m, MonadKucipongDb m, MonadTime m)
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
  redirect $ renderRoute adminUrlPrefix
  where
    noAdminLoginTokenError :: ActionCtxT ctx m a
    noAdminLoginTokenError = do
      setStatus forbidden403
      $(renderTemplateFromEnv "adminUser_login.html") $
        fromPairs
          ["errors" .= ["Failed to log in X(\nPlease try again." :: Text]]
    tokenExpiredError :: ActionCtxT ctx m a
    tokenExpiredError = do
      setStatus forbidden403
      $(renderTemplateFromEnv "adminUser_login.html") $
        fromPairs
          [ "errors" .=
            ["This log in URL has been expired X(\nPlease try again." :: Text]
          ]

-- | Return the store create page for an admin.
storeCreateGet
  :: forall xs n m.
     (ContainsAdminSession n xs, MonadIO m)
  => ActionCtxT (HVect xs) m ()
storeCreateGet = do
  (AdminSession email) <- getAdminEmail
  $(renderTemplateFromEnv "adminUser_admin_store_create.html") $
    fromPairs ["adminEmail" .= email]

storeCreatePost
  :: forall xs m.
     (MonadIO m, MonadKucipongDb m, MonadKucipongSendEmail m)
  => ActionCtxT (HVect xs) m ()
storeCreatePost = do
  (AdminStoreCreateForm storeEmailParam) <- getReqParamErr handleErr
  eitherStoreEmailEntity <- dbCreateStoreEmail storeEmailParam
  (Entity storeEmailKey storeEmail) <-
    fromEitherM handleCreateStoreFail eitherStoreEmailEntity
  (Entity _ storeLoginToken) <- dbCreateStoreMagicLoginToken storeEmailKey
  sendStoreLoginEmail
    (storeEmailEmail storeEmail)
    (storeLoginTokenLoginToken storeLoginToken)
  redirect . renderRoute $ adminUrlPrefix <//> storeCreateR
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg =
      $(renderTemplateFromEnv "adminUser_admin_store_create.html") $
      fromPairs ["errors" .= [errMsg]]

    handleCreateStoreFail :: DbSafeError -> ActionCtxT (HVect xs) m a
    handleCreateStoreFail DbSafeUniquenessViolation =
        handleErr "store with that email address already exists"
    handleCreateStoreFail _ =
        handleErr "problem with database"

adminAuthHook
  :: (MonadIO m, MonadKucipongCookie m)
  => ActionCtxT (HVect xs) m (HVect ((Session Kucipong.Session.Admin) ': xs))
adminAuthHook = do
  maybeAdminSession <- getAdminCookie
  case maybeAdminSession of
    Nothing ->
      $(renderTemplateFromEnv "adminUser_login.html") $
      fromPairs
        [ "errors" .=
          [ "Need to be logged in as admin in order to access this page." :: Text
          ]
        ]
    Just adminSession -> do
      oldCtx <- getContext
      return $ adminSession :&: oldCtx

adminComponent
  :: forall m xs.
     ( MonadIO m
     , MonadKucipongCookie m
     , MonadKucipongDb m
     , MonadKucipongSendEmail m
     , MonadTime m
     )
  => SpockCtxT (HVect xs) m ()
adminComponent = do
  get doLoginR doLoginGet
  get loginR loginGet
  post loginR loginPost
  prehook adminAuthHook $ do
    get rootR storeCreateGet
    get storeCreateR storeCreateGet
    post storeCreateR storeCreatePost

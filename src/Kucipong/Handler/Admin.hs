{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Admin where

import Kucipong.Prelude

import Kucipong.Handler.Admin.Types (AdminError(..), AdminMsg(..))

import Control.FromSum (fromEitherM, fromMaybeM, fromMaybeOrM)
import Control.Monad.Time (MonadTime(..))
import Data.Default (def)
import Data.HVect (HVect(..))
import Database.Persist (Entity(..))
import Network.HTTP.Types (forbidden403)
import Text.Heterocephalus (overwrite)
import Web.Routing.Combinators (PathState(Open))
import Web.Spock
       (ActionCtxT, Path, (<//>), getContext, redirect, renderRoute,
        setStatus, var)
import Web.Spock.Core (SpockCtxT, get, post, prehook)

import Kucipong.Db
       (DbSafeError(..), Key(..), LoginTokenExpirationTime(..),
        AdminLoginToken(adminLoginTokenExpirationTime,
                        adminLoginTokenLoginToken),
        Store(Store, storeName), StoreEmail(storeEmailEmail),
        StoreLoginToken(storeLoginTokenLoginToken))
import Kucipong.Email (EmailError)
import Kucipong.Form
       (AdminLoginForm(AdminLoginForm),
        AdminStoreCreateForm(AdminStoreCreateForm),
        AdminStoreDeleteForm(AdminStoreDeleteForm),
        AdminStoreDeleteConfirmForm(AdminStoreDeleteConfirmForm))
import Kucipong.Handler.Route
       (adminR, adminLoginR, adminLoginVarR, adminStoreCreateR,
        adminStoreDeleteR, adminStoreDeleteConfirmR)
import Kucipong.I18n (label)
import Kucipong.LoginToken (LoginToken)
import Kucipong.Monad
       (MonadKucipongCookie, MonadKucipongDb(..),
        MonadKucipongSendEmail(..), StoreDeleteResult(..), dbFindAdmin,
        dbFindAdminLoginToken, dbFindStoreByEmail)
import Kucipong.RenderTemplate
       (renderTemplate, renderTemplateFromEnv)
import Kucipong.Session (Admin, Session(..))
import Kucipong.Spock
       (ContainsAdminSession, getAdminCookie, getAdminEmail,
        getReqParamErr, setAdminCookie)

-- | Handler for returning the admin login page.
loginGet
  :: forall ctx m.
     (MonadIO m)
  => ActionCtxT ctx m ()
loginGet = $(renderTemplateFromEnv "adminUser_login.html")

-- | Handler for sending an email to the admin that they can use to login.
loginPost
  :: forall xs m.
     (MonadIO m, MonadKucipongDb m, MonadKucipongSendEmail m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
loginPost = do
  (AdminLoginForm email) <- getReqParamErr handleErr
  maybeAdminEntity <- dbFindAdmin email
  (Entity adminKey _) <-
    fromMaybeM (handleErr $ label def AdminErrorNoAdminEmail) maybeAdminEntity
  (Entity _ adminLoginToken) <- dbCreateAdminMagicLoginToken adminKey
  maybe (pure ()) handleSendEmailFail =<<
    sendAdminLoginEmail email (adminLoginTokenLoginToken adminLoginToken)
  let messages = [label def AdminMsgSentVerificationEmail]
  $(renderTemplateFromEnv "adminUser_login.html")
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $ "got following error in admin loginPost handler: " <> errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv "adminUser_login.html")
    handleSendEmailFail :: EmailError -> ActionCtxT (HVect xs) m a
    handleSendEmailFail emailError = do
      $(logDebug) $ "got email error in admin loginPost: " <> tshow emailError
      handleErr (label def AdminErrorCouldNotSendEmail)

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
  redirect $ renderRoute adminR
  where
    noAdminLoginTokenError :: ActionCtxT ctx m a
    noAdminLoginTokenError = do
      setStatus forbidden403
      let errors = [label def AdminErrorNoAdminLoginToken]
      $(renderTemplateFromEnv "adminUser_login.html")
    tokenExpiredError :: ActionCtxT ctx m a
    tokenExpiredError = do
      setStatus forbidden403
      let errors = [label def AdminErrorTokenExpired]
      $(renderTemplateFromEnv "adminUser_login.html")

-- | Return the store create page for an admin.
storeCreateGet
  :: forall xs n m.
     (ContainsAdminSession n xs, MonadIO m)
  => ActionCtxT (HVect xs) m ()
storeCreateGet = do
  _ <- getAdminEmail
  $(renderTemplateFromEnv "adminUser_admin_store_create.html")

storeCreatePost
  :: forall xs m.
     (MonadIO m, MonadKucipongDb m, MonadKucipongSendEmail m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
storeCreatePost = do
  (AdminStoreCreateForm storeEmailParam) <- getReqParamErr handleErr
  eitherStoreEmailEntity <- dbCreateStoreEmail storeEmailParam
  (Entity storeEmailKey storeEmail) <-
    fromEitherM handleCreateStoreFail eitherStoreEmailEntity
  (Entity _ storeLoginToken) <- dbCreateStoreMagicLoginToken storeEmailKey
  maybe (pure ()) handleSendEmailFail =<<
    sendStoreLoginEmail
      (storeEmailEmail storeEmail)
      (storeLoginTokenLoginToken storeLoginToken)
  redirect $ renderRoute adminStoreCreateR
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $
        "got following error in admin storeCreatePost handler: " <> errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv "adminUser_admin_store_create.html")
    handleCreateStoreFail :: DbSafeError -> ActionCtxT (HVect xs) m a
    handleCreateStoreFail DbSafeUniquenessViolation =
      handleErr $ label def AdminErrorStoreWithSameEmailExists
    handleCreateStoreFail _ =
      handleErr $ label def AdminErrorStoreCreateDbProblem
    handleSendEmailFail :: EmailError -> ActionCtxT (HVect xs) m a
    handleSendEmailFail emailError = do
      $(logDebug) $
        "got email error in admin storeCreatePost: " <> tshow emailError
      handleErr $ label def AdminErrorSendEmailFailure

-- | Return the store create page for an admin.
storeDeleteGet
  :: forall xs m.
     MonadIO m
  => ActionCtxT (HVect xs) m ()
storeDeleteGet = $(renderTemplateFromEnv "adminUser_admin_store_delete.html")

-- | Return the store delete confirmation page for an admin.
storeDeleteConfirmPost
  :: forall xs m.
     (MonadIO m, MonadKucipongDb m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
storeDeleteConfirmPost = do
  (AdminStoreDeleteConfirmForm storeEmailParam) <- getReqParamErr handleErr
  maybeStoreEntity <- dbFindStoreByEmail storeEmailParam
  (Entity _ Store {storeName}) <-
    fromMaybeOrM maybeStoreEntity $ handleErr $ label def AdminErrorNoStoreEmail
  $(renderTemplate "adminUser_admin_store_delete_confirm.html" $
    overwrite "storeName" [|storeName|])
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $
        "got following error in admin storeDeleteConfirmPost handler: " <>
        errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv "adminUser_admin_store_delete.html")

storeDeletePost
  :: forall xs m.
     (MonadIO m, MonadKucipongDb m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
storeDeletePost = do
  (AdminStoreDeleteForm storeEmailParam storeNameParam) <-
    getReqParamErr handleErr
  deleteStoreResult <- dbDeleteStoreIfNameMatches storeEmailParam storeNameParam
  case deleteStoreResult of
    res@StoreDeleteSuccess ->
      let messages = [label def res]
      in $(renderTemplateFromEnv "adminUser_admin_store_create.html")
    res@(StoreDeleteErrDoesNotExist _) -> handleErr $ label def res
    res@(StoreDeleteErrNameDoesNotMatch realStore _) ->
      let storeName' = storeName realStore
          errors = [label def res]
      in $(renderTemplate "adminUser_admin_store_delete_confirm.html" $
        overwrite "storeName" [|storeName'|])
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $
        "got following error in admin storeDeletePost handler: " <> errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv "adminUser_admin_store_delete.html")

adminAuthHook
  :: (MonadIO m, MonadKucipongCookie m)
  => ActionCtxT (HVect xs) m (HVect ((Session Kucipong.Session.Admin) ': xs))
adminAuthHook = do
  maybeAdminSession <- getAdminCookie
  case maybeAdminSession of
    Nothing ->
      let errors = [label def AdminErrorNoAdminSession]
      in $(renderTemplateFromEnv "adminUser_login.html")
    Just adminSession -> do
      oldCtx <- getContext
      return $ adminSession :&: oldCtx

adminComponent
  :: forall m xs.
     ( MonadIO m
     , MonadKucipongCookie m
     , MonadKucipongDb m
     , MonadKucipongSendEmail m
     , MonadLogger m
     , MonadTime m
     )
  => SpockCtxT (HVect xs) m ()
adminComponent = do
  get adminLoginVarR doLoginGet
  get adminLoginR loginGet
  post adminLoginR loginPost
  prehook adminAuthHook $ do
    get adminR storeCreateGet
    get adminStoreCreateR storeCreateGet
    post adminStoreCreateR storeCreatePost
    get adminStoreDeleteR storeDeleteGet
    post adminStoreDeleteR storeDeletePost
    post adminStoreDeleteConfirmR storeDeleteConfirmPost

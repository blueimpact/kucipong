{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Admin where

import Kucipong.Prelude

import Control.FromSum (fromMaybeM, fromMaybeOrM)
import Control.Monad.Time (MonadTime(..))
import Data.Default (def)
import Data.HVect (HVect(..))
import Database.Persist (Entity(..))
import Network.HTTP.Types (forbidden403)
import Text.EmailAddress (toText)
import Web.Spock
       (ActionCtxT, getContext, redirect, renderRoute, setStatus)
import Web.Spock.Core (SpockCtxT, get, post, prehook)

import Kucipong.Db
       (Key(..), LoginTokenExpirationTime(..),
        AdminLoginToken(adminLoginTokenExpirationTime,
                        adminLoginTokenLoginToken),
        Store(Store, storeName, storeEmail),
        StoreLoginToken(storeLoginTokenLoginToken))
import Kucipong.Email (EmailError)
import Kucipong.Form
       (AdminLoginForm(AdminLoginForm),
        AdminStoreCreateForm(AdminStoreCreateForm),
        AdminStoreDeleteForm(AdminStoreDeleteForm),
        AdminStoreDeleteConfirmForm(AdminStoreDeleteConfirmForm),
        AdminStoreLoginForm(AdminStoreLoginForm))
import Kucipong.Handler.Admin.TemplatePath
import Kucipong.Handler.Admin.Types (AdminError(..), AdminMsg(..))
import Kucipong.Handler.Route
       (adminR, adminLoginR, adminLoginVarR, adminStoreCreateR,
        adminStoreDeleteR, adminStoreDeleteConfirmR, adminStoreLoginR,
        storeR)
import Kucipong.I18n (label)
import Kucipong.LoginToken (LoginToken)
import Kucipong.Monad
       (MonadKucipongCookie, MonadKucipongDb(..),
        MonadKucipongSendEmail(..), StoreDeleteResult(..),
        dbCreateInitStore, dbFindAdmin, dbFindAdminLoginToken,
        dbFindStoreByEmail)
import Kucipong.RenderTemplate (renderTemplateFromEnv)
import Kucipong.Session (Session(..), SessionType(SessionTypeAdmin))
import Kucipong.Spock
       (ContainsAdminSession, getAdminCookie, getAdminEmail,
        getReqParamErr, setAdminCookie, setStoreCookie)

-- | Handler for returning the admin login page.
loginGet
  :: forall ctx m.
     (MonadIO m)
  => ActionCtxT ctx m ()
loginGet = $(renderTemplateFromEnv templateLogin)

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
  $(renderTemplateFromEnv templateLogin)
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $ "got following error in admin loginPost handler: " <> errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv templateLogin)
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
      $(renderTemplateFromEnv templateLogin)
    tokenExpiredError :: ActionCtxT ctx m a
    tokenExpiredError = do
      setStatus forbidden403
      let errors = [label def AdminErrorTokenExpired]
      $(renderTemplateFromEnv templateLogin)

-- | Return the store create page for an admin.
loginAsStoreGet
  :: forall xs m.
     MonadIO m
  => ActionCtxT (HVect xs) m ()
loginAsStoreGet = $(renderTemplateFromEnv templateStoreLogin)

loginAsStorePost
  :: forall xs m.
     ( MonadIO m
     , MonadKucipongCookie m
     , MonadKucipongDb m
     , MonadLogger m
     )
  => ActionCtxT (HVect xs) m ()
loginAsStorePost = do
  (AdminStoreLoginForm storeEmailParam) <- getReqParamErr handleErr
  maybeStoreEntity <- dbFindStoreByEmail storeEmailParam
  (Entity storeKey _) <-
    fromMaybeM (handleErr $ label def AdminErrorNoStoreEmail) maybeStoreEntity
  setStoreCookie storeKey
  redirect $ renderRoute storeR
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $
        "got following error in admin loginAsStorePost handler: " <> errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv templateStoreLogin)

-- | Return the store create page for an admin.
storeCreateGet
  :: forall xs n m.
     (ContainsAdminSession n xs, MonadIO m)
  => ActionCtxT (HVect xs) m ()
storeCreateGet = do
  _ <- getAdminEmail
  $(renderTemplateFromEnv templateStoreCreate)

storeCreatePost
  :: forall xs m.
     (MonadIO m, MonadKucipongDb m, MonadKucipongSendEmail m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
storeCreatePost = do
  (AdminStoreCreateForm storeEmailParam) <- getReqParamErr handleErr
  maybeStoreEntity <- dbCreateInitStore storeEmailParam
  (Entity storeKey Store{storeEmail}) <-
    fromMaybeM handleCreateStoreFail maybeStoreEntity
  (Entity _ storeLoginToken) <- dbCreateStoreMagicLoginToken storeKey
  maybe (pure ()) handleSendEmailFail =<<
    sendStoreLoginEmail
      storeEmail
      (storeLoginTokenLoginToken storeLoginToken)
  let messages = [label def AdminMsgCreateStoreSuccess]
  $(renderTemplateFromEnv templateStoreCreate)
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $
        "got following error in admin storeCreatePost handler: " <> errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv templateStoreCreate)

    handleCreateStoreFail :: ActionCtxT (HVect xs) m a
    handleCreateStoreFail =
      handleErr $ label def AdminErrorStoreWithSameEmailExists

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
storeDeleteGet = $(renderTemplateFromEnv templateStoreDelete)

-- | Return the store delete confirmation page for an admin.
storeDeleteConfirmPost
  :: forall xs m.
     (MonadIO m, MonadKucipongDb m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
storeDeleteConfirmPost = do
  (AdminStoreDeleteConfirmForm storeEmailParam) <- getReqParamErr handleErr
  maybeStoreEntity <- dbFindStoreByEmail storeEmailParam
  (Entity _ Store {storeName, storeEmail}) <-
    fromMaybeOrM maybeStoreEntity . handleErr $ label def AdminErrorNoStoreEmail
  $(renderTemplateFromEnv templateStoreDeleteConfirm)
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $
        "got following error in admin storeDeleteConfirmPost handler: " <>
        errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv templateStoreDelete)

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
      in $(renderTemplateFromEnv templateStoreCreate)
    res@(StoreDeleteErrDoesNotExist _) -> handleErr $ label def res
    res@(StoreDeleteErrNameDoesNotMatch Store {storeName, storeEmail} _) ->
      let errors = [label def res]
      in $(renderTemplateFromEnv templateStoreDeleteConfirm)
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $
        "got following error in admin storeDeletePost handler: " <> errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv templateStoreDelete)

adminAuthHook
  :: (MonadIO m, MonadKucipongCookie m)
  => ActionCtxT (HVect xs) m (HVect ((Session 'SessionTypeAdmin) ': xs))
adminAuthHook = do
  maybeAdminSession <- getAdminCookie
  case maybeAdminSession of
    Nothing ->
      let errors = [label def AdminErrorNoAdminSession]
      in $(renderTemplateFromEnv templateLogin)
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
    get adminStoreLoginR loginAsStoreGet
    post adminStoreLoginR loginAsStorePost

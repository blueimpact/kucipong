{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store
  ( module Kucipong.Handler.Store
  , module Kucipong.Handler.Store.Route
  ) where

import Kucipong.Prelude

import Control.FromSum (fromMaybeM)
import Control.Monad.Time (MonadTime(..))
import Data.Default (def)
import Data.List (nub)
import Data.HVect (HVect(..))
import Database.Persist (Entity(..))
import Web.Spock
       (ActionCtxT, UploadedFile(..), (<//>), files, getContext, params,
        prehook, redirect, renderRoute)
import Web.Spock.Core (SpockCtxT, get, post)

import Kucipong.Db
       (BusinessCategory(..), BusinessCategoryDetail(..), Key(..),
        LoginTokenExpirationTime(..), Store(..),
        StoreLoginToken(storeLoginTokenExpirationTime,
                        storeLoginTokenLoginToken),
        isValidBusinessCategoryDetailFor, readBusinessCategory,
        unfoldAllBusinessCategoryDetailAlt)
import Kucipong.Email (EmailError)
import Kucipong.Form
       (StoreEditForm(..), StoreLoginForm(StoreLoginForm))
import Kucipong.Handler.Store.Coupon (storeCouponComponent)
import Kucipong.Handler.Store.Route (doLoginR, editR, loginR, rootR, storeUrlPrefix)
import Kucipong.Handler.Store.Types (StoreError(..), StoreMsg(..))
import Kucipong.I18n (label)
import Kucipong.LoginToken (LoginToken)
import Kucipong.Monad
       (MonadKucipongCookie, MonadKucipongDb(..),
        MonadKucipongSendEmail(..), dbFindStoreByEmail,
        dbFindStoreLoginToken, dbUpsertStore)
import Kucipong.RenderTemplate
       (fromParams, renderTemplate, renderTemplateFromEnv)
import Kucipong.Session (Store, Session(..))
import Kucipong.Spock
       (ContainsStoreSession, getReqParamErr, getStoreCookie,
        getStoreEmail, setStoreCookie)

-- | Handler for returning the store login page.
loginGet
  :: forall ctx m.
     (MonadIO m)
  => ActionCtxT ctx m ()
loginGet = $(renderTemplateFromEnv "storeUser_login.html")

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
      (handleErr $ label def (StoreErrorNoStoreEmail email))
      maybeStoreEntity
  (Entity _ storeLoginToken) <- dbCreateStoreMagicLoginToken storeEmailKey
  maybe (pure ()) handleSendEmailFail =<<
    sendStoreLoginEmail email (storeLoginTokenLoginToken storeLoginToken)
  let messages = [label def StoreMsgSentVerificationEmail]
  $(renderTemplateFromEnv "storeUser_login.html")
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $ "got following error in store loginPost handler: " <> errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv "storeUser_login.html")

    handleSendEmailFail :: EmailError -> ActionCtxT (HVect xs) m a
    handleSendEmailFail emailError = do
      $(logDebug) $ "got email error in store loginPost: " <> tshow emailError
      handleErr $ label def StoreErrorCouldNotSendEmail

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
  redirect . renderRoute $ storeUrlPrefix <//> rootR
  where
    noStoreLoginTokenError :: ActionCtxT ctx m a
    noStoreLoginTokenError =
      redirect . renderRoute $ storeUrlPrefix <//> loginR
    tokenExpiredError :: ActionCtxT ctx m a
    tokenExpiredError = redirect . renderRoute $ storeUrlPrefix <//> loginR

storeGet
  :: forall xs n m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
storeGet = do
  (StoreSession email) <- getStoreEmail
  $(logDebug) $ "email: " <> tshow email
  maybeStoreEntity <- dbFindStoreByEmail email
  $(logDebug) $ "maybeStoreEntity: " <> tshow maybeStoreEntity
  maybeStore <- fmap entityVal <$> dbFindStoreByEmail email
  Store { storeName
        , storeSalesPoint
        , storeBusinessCategory
        , storeBusinessCategoryDetails
        , storeAddress
        , storePhoneNumber
        , storeBusinessHours
        , storeRegularHoliday
        , storeUrl
        } <- fromMaybeM handleNoStoreError maybeStore
  let
    name = storeName
    businessCategory = storeBusinessCategory
    businessCategoryDetails = storeBusinessCategoryDetails
    salesPoint = storeSalesPoint
    address = storeAddress
    phoneNumber = storePhoneNumber
    businessHourLines = fromMaybe [] (fmap lines storeBusinessHours)
    regularHoliday = storeRegularHoliday
    url = storeUrl
  $(renderTemplateFromEnv "storeUser_store.html")
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
  let
    name = (storeName <$> maybeStore)
    businessCategory = (storeBusinessCategory <$> maybeStore)
    businessCategoryDetails = concat (storeBusinessCategoryDetails <$> maybeStore)
    salesPoint = (maybeStore >>= storeSalesPoint)
    address = (maybeStore >>= storeAddress)
    phoneNumber = (maybeStore >>= storePhoneNumber)
    businessHourLines = maybe [] lines (maybeStore >>= storeBusinessHours)
    regularHoliday = (maybeStore >>= storeRegularHoliday)
    url = (maybeStore >>= storeUrl)
  $(renderTemplateFromEnv "storeUser_store_edit.html")

storeEditPost
  :: forall xs n m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
storeEditPost = do
  (StoreSession email) <- getStoreEmail
  StoreEditForm { name
                , businessCategory
                , businessCategoryDetails
                , salesPoint
                , address
                , phoneNumber
                , businessHours
                , regularHoliday
                , url
                } <- getReqParamErr handleErr
  filesHashMap <- files
  let maybeUploadedFile = lookup "image" filesHashMap
  case maybeUploadedFile of
    Just uploadedFile -> do
      let originalFileName = uf_name uploadedFile
          contentType = uf_contentType uploadedFile
          tempLocation = uf_tempLocation uploadedFile
      $(logDebug) $ "image original filename: " <> originalFileName
      $(logDebug) $ "image content type: " <> contentType
      $(logDebug) $ "image temporary location: " <> pack tempLocation
      -- upload the file to S3 here:
      -- s3UploadFile originalFileName contentType tempLocation
      pure ()
    Nothing -> handleErr $ label def StoreErrorNoImage
  checkBusinessCategoryDetails businessCategory businessCategoryDetails
  void $
    dbUpsertStore
      email
      name
      businessCategory
      (nub businessCategoryDetails)
      Nothing
      salesPoint
      address
      phoneNumber
      businessHours
      regularHoliday
      url
  redirect . renderRoute $ storeUrlPrefix
  where
    checkBusinessCategoryDetails :: BusinessCategory
                                 -> [BusinessCategoryDetail]
                                 -> ActionCtxT (HVect xs) m ()
    checkBusinessCategoryDetails busiCat busiCatDets
      | all (isValidBusinessCategoryDetailFor busiCat) busiCatDets = pure ()
      | otherwise =
        handleErr $ label def StoreErrorBusinessCategoryDetailIncorrect
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      p <- params
      $(logDebug) $ "got following error in storeEditPost handler: " <> errMsg
      let errors = [errMsg]
          businessCategory =
            readBusinessCategory =<< lookup "businessCategory" p
          businessCategoryDetails = businessCategoryDetailsFromParams p
          businessHourLines = maybe [] (lines) $ lookup "businessHours" p
      $(renderTemplate "storeUser_store_edit.html" $
        fromParams
          [|p|]
          [ "name"
          , "salesPoint"
          , "address"
          , "phoneNumber"
          , "regularHoliday"
          , "url"
          ])

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
    storeCouponComponent

allBusinessCategories :: [BusinessCategory]
allBusinessCategories = [minBound .. maxBound]

allBusinessCategoryDetails :: Maybe BusinessCategory -> [BusinessCategoryDetail]
allBusinessCategoryDetails (Just Gourmet) = map GourmetDetail [minBound .. maxBound]
allBusinessCategoryDetails (Just Fashion) = map FashionDetail [minBound .. maxBound]
allBusinessCategoryDetails (Just Gadget) = map GadgetDetail [minBound .. maxBound]
allBusinessCategoryDetails (Just Traveling) = map TravelingDetail [minBound .. maxBound]
allBusinessCategoryDetails (Just Beauty) = map BeautyDetail [minBound .. maxBound]
allBusinessCategoryDetails Nothing = map CommonDetail [minBound .. maxBound]

-- | Take the list of all parameters (which includes business category details)
-- returned from 'params', and return only those which are valid
-- 'BusinessCategoryDetail's.  Remove duplicates.
--
-- >>> let nameParam = ("name", "foo store")
-- >>> let busCatDetParam1 = ("businessCategoryDetails", "GourmetSushi")
-- >>> let busCatDetParam2 = ("businessCategoryDetails", "TravelingAsia")
-- >>> let busCatDetParam3 = ("businessCategoryDetails", "GourmetSushi")
-- >>> let busCatDetParamBad = ("businessCategoryDetails", "foobarbaz")
-- >>> let ps = [nameParam, busCatDetParam1, busCatDetParam2, busCatDetParam3, busCatDetParamBad]
-- >>> businessCategoryDetailsFromParams ps
-- [GourmetSushi,TravelingAsia]
businessCategoryDetailsFromParams :: [(Text, Text)] -> [BusinessCategoryDetail]
businessCategoryDetailsFromParams =
  nub .
  catMaybes .
  map (unfoldAllBusinessCategoryDetailAlt (Proxy :: Proxy Read) readMay) .
  filterBusinessCategoryDetails
  where
    filterBusinessCategoryDetails :: [(Text, Text)] -> [Text]
    filterBusinessCategoryDetails =
      fmap snd . filter (\(key, _) -> key == "businessCategoryDetails")

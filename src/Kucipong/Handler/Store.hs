{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store
  ( module Kucipong.Handler.Store
  ) where

import Kucipong.Prelude

import Control.FromSum (fromMaybeM)
import Control.Monad.Time (MonadTime(..))
import Data.Default (def)
import Data.List (nub)
import Data.HVect (HVect(..))
import Database.Persist (Entity(..))
import Web.Spock
       (ActionCtxT, UploadedFile(..), getContext, params, prehook,
        redirect, renderRoute)
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
import Kucipong.Handler.Route
       (storeCouponR, storeEditR, storeLoginR, storeLoginVarR, storeR)
import Kucipong.Handler.Store.Coupon (storeCouponComponent)
import Kucipong.Handler.Store.Types (StoreError(..), StoreMsg(..))
import Kucipong.Handler.Store.Util (uploadedImageToS3)
import Kucipong.I18n (label)
import Kucipong.LoginToken (LoginToken)
import Kucipong.Monad
       (FileUploadError(..), MonadKucipongAws(..), MonadKucipongCookie,
        MonadKucipongDb(..), MonadKucipongSendEmail(..),
        dbFindStoreByEmail, dbFindStoreLoginToken, dbUpsertStore)
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
  redirect $ renderRoute storeR
  where
    noStoreLoginTokenError :: ActionCtxT ctx m a
    noStoreLoginTokenError =
      redirect $ renderRoute storeLoginR
    tokenExpiredError :: ActionCtxT ctx m a
    tokenExpiredError = redirect $ renderRoute storeLoginR

storeGet
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     , MonadLogger m
     )
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
        , storeImage
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
  imageUrl <- traverse awsImageS3Url storeImage
  $(renderTemplateFromEnv "storeUser_store.html")
  where
    handleNoStoreError :: ActionCtxT (HVect xs) m a
    handleNoStoreError =
      redirect $ renderRoute storeEditR

storeEditGet
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     )
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
  imageUrl <- traverse awsImageS3Url $ maybeStore >>= storeImage
  $(renderTemplateFromEnv "storeUser_store_edit.html")

storeEditPost
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     , MonadLogger m
     )
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
  checkBusinessCategoryDetails businessCategory businessCategoryDetails
  s3ImageName <-
    uploadedImageToS3
      (handleErr $ label def StoreErrorNoImage)
      handleFileUploadError
  void $
    dbUpsertStore
      email
      name
      businessCategory
      (nub businessCategoryDetails)
      (Just s3ImageName)
      salesPoint
      address
      phoneNumber
      businessHours
      regularHoliday
      url
  redirect $ renderRoute storeR
  where
    checkBusinessCategoryDetails :: BusinessCategory
                                 -> [BusinessCategoryDetail]
                                 -> ActionCtxT (HVect xs) m ()
    checkBusinessCategoryDetails busiCat busiCatDets
      | all (isValidBusinessCategoryDetailFor busiCat) busiCatDets = pure ()
      | otherwise =
        handleErr $ label def StoreErrorBusinessCategoryDetailIncorrect

    handleFileUploadError :: UploadedFile
                          -> FileUploadError
                          -> ActionCtxT (HVect xs) m a
    handleFileUploadError uploadedFile (AwsError err) = do
      $(logDebug) $ "got following aws error in storeEditPost handler: " <> tshow err
      $(logDebug) $ "uploaded file: " <> tshow uploadedFile
      handleErr $ label def StoreErrorCouldNotUploadImage
    handleFileUploadError uploadedFile FileContentTypeError = do
      $(logDebug) "got a content type error in storeEditPost handler."
      $(logDebug) $ "uploaded file: " <> tshow uploadedFile
      handleErr $ label def StoreErrorNotAnImage
    handleFileUploadError uploadedFile (FileReadError err) = do
      $(logDebug) $ "got following error trying to read the uploaded file " <>
        "in storeEditPost handler: " <> tshow err
      $(logDebug) $ "uploaded file: " <> tshow uploadedFile
      handleErr $ label def StoreErrorCouldNotUploadImage

    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      (StoreSession email) <- getStoreEmail
      maybeStore <- fmap entityVal <$> dbFindStoreByEmail email
      imageUrl <- traverse awsImageS3Url $ maybeStore >>= storeImage
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
    Nothing -> redirect $ renderRoute storeLoginR
    Just storeSession -> do
      oldCtx <- getContext
      return $ storeSession :&: oldCtx

storeComponent
  :: forall m xs.
     ( MonadIO m
     , MonadKucipongAws m
     , MonadKucipongCookie m
     , MonadKucipongDb m
     , MonadKucipongSendEmail m
     , MonadLogger m
     , MonadTime m
     )
  => SpockCtxT (HVect xs) m ()
storeComponent = do
  get storeLoginVarR doLogin
  get storeLoginR loginGet
  post storeLoginR loginPost
  prehook storeAuthHook $ do
    get storeR storeGet
    get storeEditR storeEditGet
    post storeEditR storeEditPost
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

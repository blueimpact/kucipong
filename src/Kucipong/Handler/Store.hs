{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store
  ( module Kucipong.Handler.Store
  ) where

import Kucipong.Prelude

import Control.FromSum (fromEitherM, fromMaybeM)
import Control.Monad.Time (MonadTime(..))
import Data.Aeson (ToJSON(toJSON), Value(String))
import Data.Default (def)
import Data.List (nub)
import Data.HVect (HVect(..))
import Database.Persist (Entity(..))
import Network.HTTP.Types.Status
       (badRequest400, serviceUnavailable503)
import Web.Spock
       (ActionCtxT, getContext, params, prehook, redirect, renderRoute)
import Web.Spock.Core
       (SpockCtxT, get, files, jsonBody', post)

import Kucipong.Db
       (BusinessCategory(..), BusinessCategoryDetail(..), Key(..),
        LoginTokenExpirationTime(..), Store(..),
        StoreLoginToken(storeLoginTokenExpirationTime,
                        storeLoginTokenLoginToken),
        isValidBusinessCategoryDetailFor,
        unfoldAllBusinessCategoryDetailAlt)
import Kucipong.Email (EmailError)
import Kucipong.Form
       (StoreEditForm(..), StoreLoginForm(StoreLoginForm),
        StoreSetImageForm(StoreSetImageForm))
import Kucipong.Handler.Error (resp404)
import Kucipong.Handler.Route
       (storeCouponR, storeEditR, storeImageR, storeLoginR,
        storeLoginVarR, storeR, storeSetImageR)
import Kucipong.Handler.Store.Coupon (storeCouponComponent)
import Kucipong.Handler.Store.TemplatePath
       (templateLogin, templateStore, templateStoreEdit)
import Kucipong.Handler.Store.Types
       (StoreError(..), StoreMsg(..), StoreViewText(..),
        StoreViewTexts(..), StoreViewBusinessCategory(..),
        StoreViewBusinessCategoryDetails(..))
import Kucipong.Handler.Store.Util
       (awsUrlFromMaybeImageKey, guardMaybeImageKeyOwnedByStore)
import Kucipong.I18n (label)
import Kucipong.LoginToken (LoginToken)
import Kucipong.Monad
       (FileUploadError(..), MonadKucipongAws(..), MonadKucipongCookie,
        MonadKucipongDb(..), MonadKucipongSendEmail(..),
        dbFindStoreByEmail, dbFindStoreByStoreKey, dbFindStoreLoginToken,
        dbInsertImage, dbUpdateStore, dbUpdateStoreImage)
import Kucipong.RenderTemplate
       (renderTemplateFromEnv)
import Kucipong.Session (Session, SessionType(SessionTypeStore))
import Kucipong.Spock
       (pattern StoreSession, ContainsStoreSession, getReqParamErr,
        getStoreCookie, getStoreKey, jsonErrorStatus, jsonSuccess,
        setStoreCookie)
import Kucipong.View.Instance (ImageUrl(..))

-- | Handler for returning the store login page.
loginGet
  :: forall ctx m.
     (MonadIO m)
  => ActionCtxT ctx m ()
loginGet = $(renderTemplateFromEnv templateLogin)

-- | Handler for sending an email to the store owner that they can use to
-- login.
loginPost
  :: forall xs m.
     (MonadIO m, MonadKucipongDb m, MonadKucipongSendEmail m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
loginPost = do
  (StoreLoginForm email) <- getReqParamErr handleErr
  maybeStoreEntity <- dbFindStoreByEmail email
  (Entity storeKey _) <-
    fromMaybeM
      (handleErr $ label def (StoreErrorNoStoreEmail email))
      maybeStoreEntity
  (Entity _ storeLoginToken) <- dbCreateStoreMagicLoginToken storeKey
  maybe (pure ()) handleSendEmailFail =<<
    sendStoreLoginEmail email (storeLoginTokenLoginToken storeLoginToken)
  let messages = [label def StoreMsgSentVerificationEmail]
  $(renderTemplateFromEnv templateLogin)
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      $(logDebug) $ "got following error in store loginPost handler: " <> errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv templateLogin)

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
  (Entity (StoreLoginTokenKey storeKey) storeLoginToken) <-
    fromMaybeM noStoreLoginTokenError maybeStoreLoginTokenEntity
  -- check date on store login token
  now <- currentTime
  let (LoginTokenExpirationTime expirationTime) =
        storeLoginTokenExpirationTime storeLoginToken
  when (now > expirationTime) tokenExpiredError
  setStoreCookie storeKey
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
     )
  => ActionCtxT (HVect xs) m ()
storeGet = do
  (StoreSession storeKey) <- getStoreKey
  maybeStoreEntity <- dbFindStoreByStoreKey storeKey
  store <-
    fromMaybeM (resp404 [label def StoreErrorNoStore]) maybeStoreEntity
  let maybeImageKey = storeImage $ entityVal store
  imageUrl <- awsUrlFromMaybeImageKey maybeImageKey
  $(renderTemplateFromEnv templateStore)

storeEditGet
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongDb m
     )
  => ActionCtxT (HVect xs) m ()
storeEditGet = do
  (StoreSession storeKey) <- getStoreKey
  maybeStoreEntity <- dbFindStoreByStoreKey storeKey
  store <-
    fromMaybeM (resp404 [label def StoreErrorNoStore]) maybeStoreEntity
  $(renderTemplateFromEnv templateStoreEdit)

data ImgPostErr
  = ImgPostErrNoImg
  | ImgPostErr FileUploadError
  deriving (Show, Typeable)

instance ToJSON ImgPostErr where
  toJSON :: ImgPostErr -> Value
  toJSON ImgPostErrNoImg = String "ImgPostErrNoImg"
  toJSON (ImgPostErr fileUploadError) = toJSON fileUploadError

storeImagePost
  :: forall n xs m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     )
  => ActionCtxT (HVect xs) m ()
storeImagePost = do
  (StoreSession storeKey) <- getStoreKey
  filesHashMap <- files
  print filesHashMap
  uploadedImage <-
    fromMaybeM (handleErr ImgPostErrNoImg) $ lookup "image" filesHashMap
  eitherImageName <- first ImgPostErr <$> awsS3PutUploadedFile uploadedImage
  s3ImageName <- fromEitherM handleErr eitherImageName
  (Entity imageKey _) <- dbInsertImage storeKey s3ImageName
  jsonSuccess imageKey
  where
    handleErr :: ImgPostErr -> ActionCtxT (HVect xs) m a
    handleErr err@ImgPostErrNoImg =
      jsonErrorStatus badRequest400 err "image not found in upload"
    handleErr err@(ImgPostErr (AwsError _)) =
      jsonErrorStatus serviceUnavailable503 err "error with aws"
    handleErr err@(ImgPostErr FileContentTypeError) =
      jsonErrorStatus
        badRequest400
        err
        "content type of the image is not supported"
    handleErr err@(ImgPostErr (FileReadError _)) =
      jsonErrorStatus
        serviceUnavailable503
        err
        "server error with reading the file uploaded"

storeSetImagePost
  :: forall n xs m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongDb m
     )
  => ActionCtxT (HVect xs) m ()
storeSetImagePost = do
  (StoreSession storeKey) <- getStoreKey
  StoreSetImageForm maybeImageKey <- jsonBody'
  guardMaybeImageKeyOwnedByStore storeKey maybeImageKey handleErr
  void $ dbUpdateStoreImage storeKey maybeImageKey
  jsonSuccess maybeImageKey
  where
    handleErr :: ActionCtxT (HVect xs) m a
    handleErr =
      jsonErrorStatus
        badRequest400
        StoreErrorImageOwnedByStore
        (label def StoreErrorImageOwnedByStore)

storeEditPost
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongDb m
     , MonadLogger m
     )
  => ActionCtxT (HVect xs) m ()
storeEditPost = do
  (StoreSession storeKey) <- getStoreKey
  StoreEditForm { name
                , businessCategory
                , businessCategoryDetails
                , imageKey
                , salesPoint
                , address
                , phoneNumber
                , businessHours
                , regularHoliday
                , url
                } <- getReqParamErr handleErr
  let businessCategoryDetails' =
        filterBusinessCategoryDetails businessCategory businessCategoryDetails
  guardMaybeImageKeyOwnedByStore storeKey imageKey $
    handleErr (label def StoreErrorImageOwnedByStore)
  void $
    dbUpdateStore
      storeKey
      name
      businessCategory
      (nub businessCategoryDetails')
      imageKey
      salesPoint
      address
      phoneNumber
      businessHours
      regularHoliday
      url
  redirect $ renderRoute storeR
  where
    filterBusinessCategoryDetails
      :: Maybe BusinessCategory
      -> [BusinessCategoryDetail]
      -> [BusinessCategoryDetail]
    filterBusinessCategoryDetails Nothing _ = []
    filterBusinessCategoryDetails (Just busiCat) busiCatDets =
      filter (isValidBusinessCategoryDetailFor busiCat) busiCatDets

    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      store <- params
      $(logDebug) $ "got following error in storeEditPost handler: " <> errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv templateStoreEdit)

storeAuthHook
  :: (MonadIO m, MonadKucipongCookie m)
  => ActionCtxT (HVect xs) m (HVect ((Session 'SessionTypeStore) ': xs))
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
    post storeImageR storeImagePost
    post storeSetImageR storeSetImagePost
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

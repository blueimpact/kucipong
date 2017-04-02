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
       (ActionCtxT, getContext, params, prehook, redirect, renderRoute)
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
import Kucipong.Handler.Store.TemplatePath
import Kucipong.Handler.Store.Types
       (StoreError(..), StoreMsg(..), StoreView(..), StoreViewText(..),
        StoreViewTexts(..), StoreViewBusinessCategory(..),
        StoreViewBusinessCategoryDetails(..), StoreViewImageUrl(..),
        StoreViewDefaultImage(..))
import Kucipong.I18n (label)
import Kucipong.LoginToken (LoginToken)
import Kucipong.Monad
       (MonadKucipongAws(..), MonadKucipongCookie,
        MonadKucipongDb(..), MonadKucipongSendEmail(..), awsImageS3Url,
        dbFindStoreByEmail, dbFindStoreByStoreKey, dbFindStoreLoginToken,
        dbUpdateStore)
import Kucipong.RenderTemplate
       (renderTemplateFromEnv)
import Kucipong.Session (Store, Session)
import Kucipong.Spock
       (pattern StoreSession, ContainsStoreSession, getReqParamErr,
        getStoreCookie, getStoreKey, setStoreCookie)

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
  let maybeImage = storeImage . entityVal =<< maybeStoreEntity
  maybeImageUrl <- traverse awsImageS3Url maybeImage
  let
    mdata = StoreView
      <$> maybeStoreEntity
      <*> pure maybeImageUrl
      <*> pure Nothing
  $(renderTemplateFromEnv templateStore)

storeEditGet
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     )
  => ActionCtxT (HVect xs) m ()
storeEditGet = do
  (StoreSession storeKey) <- getStoreKey
  maybeStoreEntity <- dbFindStoreByStoreKey storeKey
  let maybeImage = storeImage . entityVal =<< maybeStoreEntity
  maybeImageUrl <- traverse awsImageS3Url maybeImage
  let
    mdata = StoreView
      <$> maybeStoreEntity
      <*> pure maybeImageUrl
      <*> pure Nothing
  $(renderTemplateFromEnv templateStoreEdit)

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
  (StoreSession storeKey) <- getStoreKey
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
  let businessCategoryDetails' =
        filterBusinessCategoryDetails businessCategory businessCategoryDetails
  void $
    dbUpdateStore
      storeKey
      name
      businessCategory
      (nub businessCategoryDetails')
      Nothing
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
      (StoreSession storeKey) <- getStoreKey
      maybeStoreEntity <- dbFindStoreByStoreKey storeKey
      let maybeImage = storeImage . entityVal =<< maybeStoreEntity
      maybeImageUrl <- traverse awsImageS3Url maybeImage
      p <- params
      let
        maybeStore = do
          (Entity k v) <- maybeStoreEntity
          pure . Entity k $ v
            { storeName = lookup "name" p
            , storeSalesPoint = lookup "salesPoint" p
            , storeAddress = lookup "address" p
            , storePhoneNumber = lookup "phoneNumber" p
            , storeRegularHoliday = lookup "regularHoliday" p
            , storeUrl = lookup "url" p
            , storeBusinessHours = lookup "businessHours" p
            , storeBusinessCategory =
                readBusinessCategory =<< lookup "businessCategory" p
            , storeBusinessCategoryDetails =
                 businessCategoryDetailsFromParams p
            }

        mdata = StoreView
          <$> maybeStore
          <*> pure maybeImageUrl
          <*> pure (lookup "defaultImage" p)
      $(logDebug) $ "got following error in storeEditPost handler: " <> errMsg
      let errors = [errMsg]
      $(renderTemplateFromEnv templateStoreEdit)

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

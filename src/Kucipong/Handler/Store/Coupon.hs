{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store.Coupon where

import Kucipong.Prelude

import Kucipong.Handler.Store.Types (StoreError(..), StoreMsg(..))

import Control.FromSum (fromMaybeM)
import Control.Lens ((&), (.~))
import Control.Monad.Time (MonadTime(..))
import Data.Default (def)
import Data.List (nub)
import Data.HVect (HVect(..))
import Database.Persist (Entity(..))
import Web.Routing.Combinators (PathState(Open))
import Web.Spock
       (ActionCtxT, Path, UploadedFile(..), (<//>), files, getContext,
        params, prehook, root, redirect, renderRoute, var)
import Web.Spock.Core (SpockCtxT, get, post)

import Kucipong.Db
       (BusinessCategory(..), BusinessCategoryDetail(..), CouponType(..),
        Key(..), LoginTokenExpirationTime(..), Store(..),
        StoreLoginToken(storeLoginTokenExpirationTime,
                        storeLoginTokenLoginToken),
        couponTypeToText, isValidBusinessCategoryDetailFor,
        readBusinessCategory, unfoldAllBusinessCategoryDetailAlt)
import Kucipong.Email (EmailError)
import Kucipong.Form
       (StoreNewCouponForm(..), StoreLoginForm(StoreLoginForm),
        removeNonUsedCouponInfo)
import Kucipong.Handler.Store.Route (storeUrlPrefix, couponR)
import Kucipong.I18n (label)
import Kucipong.LoginToken (LoginToken)
import Kucipong.Monad
       (MonadKucipongCookie, MonadKucipongDb(..),
        MonadKucipongSendEmail(..), dbInsertCoupon,
        dbFindStoreLoginToken, dbUpsertStore)
import Kucipong.RenderTemplate
       (fromParams, renderTemplate, renderTemplateFromEnv)
import Kucipong.Session (Store, Session(..))
import Kucipong.Spock
       (ContainsStoreSession, getReqParamErr, getStoreCookie,
        getStoreEmail, setStoreCookie)

storeGet
  :: forall xs n m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
storeGet = do
  undefined
  -- (StoreSession email) <- getStoreEmail
  -- undefined $ (logDebug) $ "email: " <> tshow email
  -- maybeStoreEntity <- dbFindStoreByEmail email
  -- undefined $ (logDebug) $ "maybeStoreEntity: " <> tshow maybeStoreEntity
  -- maybeStore <- fmap entityVal <$> dbFindStoreByEmail email
  -- Store { storeName
  --       , storeSalesPoint
  --       , storeBusinessCategory
  --       , storeBusinessCategoryDetails
  --       , storeAddress
  --       , storePhoneNumber
  --       , storeBusinessHours
  --       , storeRegularHoliday
  --       , storeUrl
  --       } <- fromMaybeM handleNoStoreError maybeStore
  -- let
  --   name = storeName
  --   businessCategory = storeBusinessCategory
  --   businessCategoryDetails = storeBusinessCategoryDetails
  --   salesPoint = storeSalesPoint
  --   address = storeAddress
  --   phoneNumber = storePhoneNumber
  --   businessHourLines = fromMaybe [] (fmap lines storeBusinessHours)
  --   regularHoliday = storeRegularHoliday
  --   url = storeUrl
  -- undefined $ (renderTemplateFromEnv "storeUser_store.html")
  -- where
  --   handleNoStoreError :: ActionCtxT (HVect xs) m a
  --   handleNoStoreError =
  --     redirect . renderRoute $ storeUrlPrefix <//> editR

storeEditGet
  :: forall xs n m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m)
  => ActionCtxT (HVect xs) m ()
storeEditGet = do
  undefined
  -- (StoreSession email) <- getStoreEmail
  -- maybeStore <- fmap entityVal <$> dbFindStoreByEmail email
  -- let
  --   name = (storeName <$> maybeStore)
  --   businessCategory = (storeBusinessCategory <$> maybeStore)
  --   businessCategoryDetails = concat (storeBusinessCategoryDetails <$> maybeStore)
  --   salesPoint = (maybeStore >>= storeSalesPoint)
  --   address = (maybeStore >>= storeAddress)
  --   phoneNumber = (maybeStore >>= storePhoneNumber)
  --   businessHourLines = maybe [] lines (maybeStore >>= storeBusinessHours)
  --   regularHoliday = (maybeStore >>= storeRegularHoliday)
  --   url = (maybeStore >>= storeUrl)
  -- undefined $ (renderTemplateFromEnv "storeUser_store_edit.html")

couponPost
  :: forall xs n m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m, MonadLogger m)
  => ActionCtxT (HVect xs) m ()
couponPost = do
  (StoreSession email) <- getStoreEmail
  storeNewCouponForm <- getReqParamErr handleErr
  let StoreNewCouponForm {..} = removeNonUsedCouponInfo storeNewCouponForm
  void $
    dbInsertCoupon
      email
      title
      couponType
      validFrom
      validUntil
      Nothing -- image
      discountPercent
      discountMinimumPrice
      discountOtherConditions
      giftContent
      giftReferencePrice
      giftMinimumPrice
      giftOtherConditions
      setContent
      setPrice
      setReferencePrice
      setOtherConditions
      otherContent
      otherConditions
  redirect . renderRoute $ storeUrlPrefix <//> couponR
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      p <- params
      $(logDebug) $ "got following error in store couponPost handler: " <> errMsg
      let errors = [errMsg]
      $(renderTemplate "storeUser_store_coupon_id_edit.html" $
        fromParams
          [|p|]
          [ "title"
          , "couponType"
          , "validFrom"
          , "validUntil"
          , "discountPercent"
          , "discountMinimumPrice"
          , "discountOtherConditions"
          , "giftContent"
          , "giftReferencePrice"
          , "giftMinimumPrice"
          , "giftOtherConditions"
          , "setContent"
          , "setPrice"
          , "setReferencePrice"
          , "setOtherConditionstions"
          , "otherContent"
          , "otherConditions"
          ])

storeCouponComponent
  :: forall m xs.
     ( MonadIO m
     , MonadKucipongCookie m
     , MonadKucipongDb m
     , MonadKucipongSendEmail m
     , MonadLogger m
     , MonadTime m
     )
  => SpockCtxT (HVect (Session Kucipong.Session.Store : xs)) m ()
storeCouponComponent = do
  post couponR couponPost
  -- get doLoginR doLogin
  -- get loginR loginGet
  -- post loginR loginPost
  -- prehook storeAuthHook $ do
  --   get rootR storeGet
  --   get editR storeEditGet
  --   post editR storeEditPost


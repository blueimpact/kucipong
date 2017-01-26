{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store.Coupon where

import Kucipong.Prelude

import Control.FromSum (fromMaybeM)
import Control.Lens (_Wrapped, view)
import Data.HVect (HVect(..))
import Database.Persist.Sql (Entity(..), fromSqlKey)
import Web.Spock
       (ActionCtxT, (<//>), params, redirect, renderRoute)
import Web.Spock.Core (SpockCtxT, get, post, var)

import Kucipong.Db
       (Coupon(..), CouponType(..), Key(..), Store(..), couponTypeToText,
        percentToText, priceToText)
import Kucipong.Form
       (StoreNewCouponForm(..), removeNonUsedCouponInfo)
import Kucipong.Handler.Store.Route
       (couponR, createR, editR, storeUrlPrefix)
import Kucipong.Monad
       (MonadKucipongDb(..), dbFindCouponByEmailAndId,
        dbFindCouponsByEmail, dbFindStoreByEmail, dbInsertCoupon)
import Kucipong.RenderTemplate
       (fromParams, renderTemplate, renderTemplateFromEnv)
import Kucipong.Session (Store, Session(..))
import Kucipong.Spock
       (ContainsStoreSession, getReqParamErr, getStoreEmail)

couponNewGet
  :: forall xs m.
     (MonadIO m)
  => ActionCtxT (HVect xs) m ()
couponNewGet = do
  let p = [] :: [(Text, Text)]
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
      , "setOtherConditions"
      , "otherContent"
      , "otherConditions"
      ])

couponGet
  :: forall xs n m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m)
  => Key Coupon -> ActionCtxT (HVect xs) m ()
couponGet couponKey = do
  (StoreSession email) <- getStoreEmail
  maybeCouponEntity <- dbFindCouponByEmailAndId email couponKey
  maybeStoreEntity <- dbFindStoreByEmail email
  $(renderTemplateFromEnv "storeUser_store_coupon_id.html")

couponEditGet
  :: forall xs n m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m, MonadLogger m)
  => Key Coupon -> ActionCtxT (HVect xs) m ()
couponEditGet couponKey = do
  (StoreSession email) <- getStoreEmail
  maybeCouponEntity <- dbFindCouponByEmailAndId email couponKey
  Entity _ (Coupon _ _ _ _ (Just -> title) (Just . couponTypeToText -> couponType) (fmap tshow -> validFrom) (fmap tshow -> validUntil) _ (fmap percentToText  -> discountPercent) (fmap priceToText -> discountMinimumPrice) discountOtherConditions giftContent (fmap priceToText -> giftReferencePrice) (fmap priceToText -> giftMinimumPrice) giftOtherConditions setContent (fmap priceToText -> setPrice) (fmap priceToText -> setReferencePrice) setOtherConditions otherContent otherConditions) <-
    fromMaybeM (handleErr "couldn't find coupon") maybeCouponEntity
  $(renderTemplateFromEnv "storeUser_store_coupon_id_edit.html")
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      p <- params
      $(logDebug) $ "params: " <> tshow p
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
          , "setOtherConditions"
          , "otherContent"
          , "otherConditions"
          ])

couponListGet
  :: forall xs n m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m)
  => ActionCtxT (HVect xs) m ()
couponListGet = do
  (StoreSession email) <- getStoreEmail
  couponEntities <- dbFindCouponsByEmail email
  $(renderTemplateFromEnv "storeUser_store_coupon.html")

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
      (view _Wrapped validFrom)
      (view _Wrapped validUntil)
      Nothing -- image
      (view _Wrapped discountPercent)
      (view _Wrapped discountMinimumPrice)
      (view _Wrapped discountOtherConditions)
      (view _Wrapped giftContent)
      (view _Wrapped giftReferencePrice)
      (view _Wrapped giftMinimumPrice)
      (view _Wrapped giftOtherConditions)
      (view _Wrapped setContent)
      (view _Wrapped setPrice)
      (view _Wrapped setReferencePrice)
      (view _Wrapped setOtherConditions)
      (view _Wrapped otherContent)
      (view _Wrapped otherConditions)
  redirect . renderRoute $ storeUrlPrefix <//> couponR
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      p <- params
      $(logDebug) $ "params: " <> tshow p
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
          , "setOtherConditions"
          , "otherContent"
          , "otherConditions"
          ])

storeCouponComponent
  :: forall m xs.
     ( MonadIO m
     , MonadKucipongDb m
     , MonadLogger m
     )
  => SpockCtxT (HVect (Session Kucipong.Session.Store : xs)) m ()
storeCouponComponent = do
  get couponR couponListGet
  post couponR couponPost
  get (couponR <//> createR) couponNewGet
  get (couponR <//> var) couponGet
  get (couponR <//> var <//> editR) couponEditGet

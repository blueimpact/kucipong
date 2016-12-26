{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store.Coupon where

import Kucipong.Prelude

import Data.HVect (HVect(..))
import Web.Spock
       (ActionCtxT, (<//>), params, redirect, renderRoute)
import Web.Spock.Core (SpockCtxT, get, post)

import Kucipong.Db (CouponType(..), couponTypeToText)
import Kucipong.Form
       (StoreNewCouponForm(..), removeNonUsedCouponInfo)
import Kucipong.Handler.Store.Route
       (storeUrlPrefix, couponR, createR)
import Kucipong.Monad (MonadKucipongDb(..), dbInsertCoupon)
import Kucipong.RenderTemplate (fromParams, renderTemplate)
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
  post couponR couponPost
  get (couponR <//> createR) couponNewGet


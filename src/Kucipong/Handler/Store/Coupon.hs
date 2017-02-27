{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store.Coupon where

import Kucipong.Prelude

import Control.FromSum (fromEitherMM, fromMaybeM)
import Control.Lens (_Wrapped, view)
import Data.Default (def)
import Data.HVect (HVect(..))
import Database.Persist.Sql (Entity(..), fromSqlKey)
import Web.Spock (ActionCtxT, params, redirect, renderRoute)
import Web.Spock.Core (SpockCtxT, get, post)

import Kucipong.Db
       (Coupon(..), CouponType(..), Key(..), Store(..), couponTypeToText,
        percentToText, priceToText)
import Kucipong.Form
       (StoreNewCouponForm(..), removeNonUsedCouponInfo)
import Kucipong.Handler.Route
       (storeCouponR, storeCouponCreateR, storeCouponVarR,
        storeCouponVarEditR, storeR)
import Kucipong.Handler.Store.Types (StoreError(..))
import Kucipong.Handler.Store.Util
       (UploadImgErr(..), uploadImgToS3WithDef)
import Kucipong.I18n (label)
import Kucipong.Monad
       (FileUploadError(..), MonadKucipongAws(..), MonadKucipongDb(..),
        awsGetBucketName, awsImageS3Url, awsUrlFromImageAndBucket, dbFindCouponByEmailAndId,
        dbFindCouponsByEmail, dbFindStoreByEmail, dbInsertCoupon,
        dbUpdateCoupon)
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
      action = renderRoute storeCouponCreateR
  $(renderTemplate "storeUser_store_coupon_id_edit.html" $
    fromParams
      [|p|]
      [ "title"
      , "couponType"
      , "validFrom"
      , "validUntil"
      , "maybeImageUrl"
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
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     )
  => Key Coupon -> ActionCtxT (HVect xs) m ()
couponGet couponKey = do
  (StoreSession email) <- getStoreEmail
  maybeCouponEntity <- dbFindCouponByEmailAndId email couponKey
  maybeStoreEntity <- dbFindStoreByEmail email
  let maybeImage = couponImage . entityVal =<< maybeCouponEntity
  maybeImageUrl <- traverse awsImageS3Url maybeImage
  $(renderTemplateFromEnv "storeUser_store_coupon_id.html")

couponEditGet
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     , MonadLogger m
     )
  => Key Coupon -> ActionCtxT (HVect xs) m ()
couponEditGet couponKey = do
  (StoreSession email) <- getStoreEmail
  maybeCouponEntity <- dbFindCouponByEmailAndId email couponKey
  Entity
    _
    (Coupon
      _
      _
      _
      _
      (Just -> title)
      (Just . couponTypeToText -> couponType)
      (fmap tshow -> validFrom)
      (fmap tshow -> validUntil)
      maybeImage
      (fmap percentToText  -> discountPercent)
      (fmap priceToText -> discountMinimumPrice)
      discountOtherConditions
      giftContent
      (fmap priceToText -> giftReferencePrice)
      (fmap priceToText -> giftMinimumPrice)
      giftOtherConditions
      setContent
      (fmap priceToText -> setPrice)
      (fmap priceToText -> setReferencePrice)
      setOtherConditions
      otherContent
      otherConditions) <-
        fromMaybeM (handleErr "couldn't find coupon") maybeCouponEntity
  maybeImageUrl <- traverse awsImageS3Url maybeImage
  let action = renderRoute storeCouponVarEditR couponKey
  $(renderTemplateFromEnv "storeUser_store_coupon_id_edit.html")
  where
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      (StoreSession email) <- getStoreEmail
      maybeCouponEntity <- dbFindCouponByEmailAndId email couponKey
      let maybeImage = maybeCouponEntity >>= couponImage . entityVal
      maybeImageUrl <- traverse awsImageS3Url maybeImage
      p <- params
      $(logDebug) $ "params: " <> tshow p
      $(logDebug) $ "got following error in store couponPost handler: " <> errMsg
      let errors = [errMsg]
          action = renderRoute storeCouponVarEditR couponKey
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

couponEditPost
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     , MonadLogger m
     )
  => Key Coupon -> ActionCtxT (HVect xs) m ()
couponEditPost couponKey = do
  (StoreSession email) <- getStoreEmail
  storeNewCouponForm <- getReqParamErr handleErr
  let StoreNewCouponForm {..} = removeNonUsedCouponInfo storeNewCouponForm
  s3ImageName <-
    fromEitherMM handleFileUploadErr $ uploadImgToS3WithDef defaultImage
  void $
    dbUpdateCoupon
      couponKey
      email
      title
      couponType
      (view _Wrapped validFrom)
      (view _Wrapped validUntil)
      s3ImageName
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
  redirect $ renderRoute storeCouponVarR couponKey
  where
    handleFileUploadErr
      :: UploadImgErr
      -> ActionCtxT (HVect xs) m a
    handleFileUploadErr (UploadImgErr uploadedFile (AwsError err)) = do
      $(logDebug) $ "got following aws error in couponEditPost handler: " <> tshow err
      $(logDebug) $ "uploaded file: " <> tshow uploadedFile
      handleErr $ label def StoreErrorCouldNotUploadImage
    handleFileUploadErr (UploadImgErr uploadedFile FileContentTypeError) = do
      $(logDebug) "got a content type error in couponEditPost handler."
      $(logDebug) $ "uploaded file: " <> tshow uploadedFile
      handleErr $ label def StoreErrorNotAnImage
    handleFileUploadErr (UploadImgErr uploadedFile (FileReadError err)) = do
      $(logDebug) $ "got following error trying to read the uploaded file " <>
        "in couponEditPost handler: " <> tshow err
      $(logDebug) $ "uploaded file: " <> tshow uploadedFile
      handleErr $ label def StoreErrorCouldNotUploadImage

    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      (StoreSession email) <- getStoreEmail
      maybeCouponEntity <- dbFindCouponByEmailAndId email couponKey
      let maybeImage = maybeCouponEntity >>= couponImage . entityVal
      maybeImageUrl <- traverse awsImageS3Url maybeImage
      p <- params
      $(logDebug) $ "params: " <> tshow p
      $(logDebug) $
        "got following error in store couponEditPost handler: " <> errMsg
      let errors = [errMsg]
      let action = renderRoute storeCouponVarEditR couponKey
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
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     )
  => ActionCtxT (HVect xs) m ()
couponListGet = do
  (StoreSession email) <- getStoreEmail
  couponEntities <- dbFindCouponsByEmail email
  bucketName <- awsGetBucketName
  let awsImageUrlFunc = fmap $ awsUrlFromImageAndBucket bucketName
  $(renderTemplateFromEnv "storeUser_store_coupon.html")

couponPost
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     , MonadLogger m
     )
  => ActionCtxT (HVect xs) m ()
couponPost = do
  (StoreSession email) <- getStoreEmail
  storeNewCouponForm <- getReqParamErr handleErr
  let StoreNewCouponForm {..} = removeNonUsedCouponInfo storeNewCouponForm
  s3ImageName <-
    fromEitherMM handleFileUploadErr $ uploadImgToS3WithDef defaultImage
  void $
    dbInsertCoupon
      email
      title
      couponType
      (view _Wrapped validFrom)
      (view _Wrapped validUntil)
      s3ImageName
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
  redirect $ renderRoute storeCouponR
  where
    handleFileUploadErr
      :: UploadImgErr
      -> ActionCtxT (HVect xs) m a
    handleFileUploadErr (UploadImgErr uploadedFile (AwsError err)) = do
      $(logDebug) $ "got following aws error in couponPost handler: " <> tshow err
      $(logDebug) $ "uploaded file: " <> tshow uploadedFile
      handleErr $ label def StoreErrorCouldNotUploadImage
    handleFileUploadErr (UploadImgErr uploadedFile FileContentTypeError) = do
      $(logDebug) "got a content type error in couponPost handler."
      $(logDebug) $ "uploaded file: " <> tshow uploadedFile
      handleErr $ label def StoreErrorNotAnImage
    handleFileUploadErr (UploadImgErr uploadedFile (FileReadError err)) = do
      $(logDebug) $ "got following error trying to read the uploaded file " <>
        "in couponPost handler: " <> tshow err
      $(logDebug) $ "uploaded file: " <> tshow uploadedFile
      handleErr $ label def StoreErrorCouldNotUploadImage

    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      p <- params
      $(logDebug) $ "params: " <> tshow p
      $(logDebug) $ "got following error in store couponPost handler: " <> errMsg
      let errors = [errMsg]
          action = renderRoute storeCouponCreateR
      $(renderTemplate "storeUser_store_coupon_id_edit.html" $
        fromParams
          [|p|]
          [ "title"
          , "couponType"
          , "validFrom"
          , "validUntil"
          -- TODO: We should make sure the image url is filled in like normal
          -- for the user when they resubmit the page.
          , "maybeImageUrl"
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
     , MonadKucipongAws m
     , MonadKucipongDb m
     , MonadLogger m
     )
  => SpockCtxT (HVect (Session Kucipong.Session.Store : xs)) m ()
storeCouponComponent = do
  get storeCouponR couponListGet
  post storeCouponCreateR couponPost
  get storeCouponCreateR couponNewGet
  get storeCouponVarR couponGet
  get storeCouponVarEditR couponEditGet
  post storeCouponVarEditR couponEditPost

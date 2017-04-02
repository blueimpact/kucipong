{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store.Coupon where

import Kucipong.Prelude

import Control.FromSum (fromEitherMM)
import Control.Lens (_Wrapped, view)
import Data.Default (def)
import Data.HVect (HVect(..))
import Database.Persist.Sql (Entity(..), fromSqlKey)
import Web.Spock (ActionCtxT, params, redirect, renderRoute)
import Web.Spock.Core
       (ClientPreferredFormat(PrefJSON), SpockCtxT, get, json,
        preferredFormat, post)

import Kucipong.Db
       (Coupon(..), CouponType(..), Key(..))
import Kucipong.Form
       (StoreNewCouponForm(..), removeNonUsedCouponInfo)
import Kucipong.Handler.Error (resp404)
import Kucipong.Handler.Route
       (storeCouponR, storeCouponCreateR, storeCouponDeleteVarR,
        storeCouponVarR, storeCouponVarEditR, storeR)
import Kucipong.Handler.Store.TemplatePath
       (templateCoupon, templateCouponCreate, templateCouponDelete, templateCouponId,
        templateCouponIdEdit)
import Kucipong.Handler.Store.Types
       (StoreError(..), CouponView(..), CouponViewImageUrl(..),
        CouponViewKey(..), CouponViewTypes(..), CouponViewConditions(..),
        CouponViewCouponType(..), StoreViewText(..))
import Kucipong.Handler.Store.Util
       (UploadImgErr(..), uploadImgToS3WithDef)
import Kucipong.Handler.Types (PageViewer(..))
import Kucipong.I18n (label)
import Kucipong.Monad
       (CouponDeleteResult(..), FileUploadError(..), MonadKucipongAws(..),
        MonadKucipongDb(..), awsGetBucketName, awsImageS3Url,
        awsUrlFromImageAndBucket, dbFindCouponByStoreKeyAndCouponKey,
        dbFindCouponsByStoreKey, dbFindStoreByStoreKey, dbInsertCoupon,
        dbUpdateCoupon)
import Kucipong.RenderTemplate (renderTemplateFromEnv)
import Kucipong.Session (Store, Session)
import Kucipong.Spock
       (pattern StoreSession, ContainsStoreSession, getReqParamErr,
        getStoreKey)
import Kucipong.View (View(..))

couponNewGet
  :: forall xs m.
     (MonadIO m)
  => ActionCtxT (HVect xs) m ()
couponNewGet = do
  let
    action = renderRoute storeCouponCreateR
    mdata = pure mempty :: Maybe [(Text, Text)]
  $(renderTemplateFromEnv templateCouponCreate)

couponGet
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     )
  => Key Coupon -> ActionCtxT (HVect xs) m ()
couponGet couponKey = do
  (StoreSession storeKey) <- getStoreKey
  maybeCouponEntity <- dbFindCouponByStoreKeyAndCouponKey storeKey couponKey
  maybeStoreEntity <- dbFindStoreByStoreKey storeKey
  let maybeImage = couponImage . entityVal =<< maybeCouponEntity
  maybeImageUrl <- traverse awsImageS3Url maybeImage
  let
    mdata = CouponView
      <$> maybeStoreEntity
      <*> maybeCouponEntity
      <*> pure maybeImageUrl
    aboutStore = renderRoute storeR
    pageViewer = PageViewerStore
  $(renderTemplateFromEnv templateCouponId)

couponEditGet
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongDb m
     )
  => Key Coupon -> ActionCtxT (HVect xs) m ()
couponEditGet couponKey = do
  (StoreSession storeKey) <- getStoreKey
  maybeCouponEntity <- dbFindCouponByStoreKeyAndCouponKey storeKey couponKey
  let
    action = renderRoute storeCouponVarEditR couponKey
    mdata = maybeCouponEntity
  $(renderTemplateFromEnv templateCouponIdEdit)

couponEditPost
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongDb m
     , MonadLogger m
     )
  => Key Coupon -> ActionCtxT (HVect xs) m ()
couponEditPost couponKey = do
  (StoreSession storeKey) <- getStoreKey
  storeNewCouponForm <- getReqParamErr handleErr
  let StoreNewCouponForm {..} = removeNonUsedCouponInfo storeNewCouponForm
  void $
    dbUpdateCoupon
      couponKey
      storeKey
      title
      couponType
      (view _Wrapped validFrom)
      (view _Wrapped validUntil)
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
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      p <- params
      $(logDebug) $ "params: " <> tshow p
      $(logDebug) $
        "got following error in store couponEditPost handler: " <> errMsg
      let errors = [errMsg]
          action = renderRoute storeCouponVarEditR couponKey
          mdata = return p
      $(renderTemplateFromEnv templateCouponIdEdit)

couponListGet
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     )
  => ActionCtxT (HVect xs) m ()
couponListGet = do
  (StoreSession storeKey) <- getStoreKey
  couponEntities <- dbFindCouponsByStoreKey storeKey
  bucketName <- awsGetBucketName
  let awsImageUrlFunc = fmap $ awsUrlFromImageAndBucket bucketName
  $(renderTemplateFromEnv templateCoupon)

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
  (StoreSession storeKey) <- getStoreKey
  storeNewCouponForm <- getReqParamErr handleErr
  let StoreNewCouponForm {..} = removeNonUsedCouponInfo storeNewCouponForm
  s3ImageName <-
    fromEitherMM handleFileUploadErr $ uploadImgToS3WithDef defaultImage
  void $
    dbInsertCoupon
      storeKey
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
          mdata = return p
      $(renderTemplateFromEnv templateCouponIdEdit)

-- | Return the coupon delete page for a store.
-- TODO: This can be deleted when
-- https://github.com/blueimpact/kucipong/issues/157 is implemented
couponDeleteGet
  :: forall xs n m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m)
  => Key Coupon -> ActionCtxT (HVect xs) m ()
couponDeleteGet couponKey = do
  (StoreSession storeKey) <- getStoreKey
  maybeCouponEntity <- dbFindCouponByStoreKeyAndCouponKey storeKey couponKey
  case maybeCouponEntity of
    Nothing -> resp404 [label def StoreErrorCouponNotFound]
    Just (Entity _ Coupon{couponTitle}) ->
      $(renderTemplateFromEnv templateCouponDelete)

-- TODO: This can be deleted when
-- https://github.com/blueimpact/kucipong/issues/157 is implemented
couponDeletePost
  :: forall n xs m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m)
  => Key Coupon -> ActionCtxT (HVect xs) m ()
couponDeletePost couponKey = do
  prefFormat <- preferredFormat
  case prefFormat of
    PrefJSON -> couponDeletePostJson couponKey
    _ -> do
      (StoreSession storeKey) <- getStoreKey
      deleteCouponResult <- dbDeleteCoupon storeKey couponKey
      case deleteCouponResult of
        CouponDeleteErrDoesNotExist ->
          resp404 [label def StoreErrorCouponNotFound]
        CouponDeleteSuccess -> redirect $ renderRoute storeCouponR

couponDeletePostJson
  :: forall n xs m.
     (ContainsStoreSession n xs, MonadIO m, MonadKucipongDb m)
  => Key Coupon -> ActionCtxT (HVect xs) m ()
couponDeletePostJson couponKey = do
  (StoreSession storeKey) <- getStoreKey
  deleteCouponResult <- dbDeleteCoupon storeKey couponKey
  json deleteCouponResult

allCouponTypes :: [CouponType]
allCouponTypes = [minBound .. maxBound]

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
  get storeCouponDeleteVarR couponDeleteGet
  post storeCouponDeleteVarR couponDeletePost
  get storeCouponVarR couponGet
  get storeCouponVarEditR couponEditGet
  post storeCouponVarEditR couponEditPost

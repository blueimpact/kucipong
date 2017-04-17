{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store.Coupon where

import Kucipong.Prelude

import Control.FromSum (fromMaybeM)
import Control.Lens (_Wrapped, view)
import Data.Default (def)
import Data.HVect (HVect(..))
import Database.Persist.Sql (Entity(..), fromSqlKey)
import Network.HTTP.Types.Status (badRequest400)
import Web.Spock (ActionCtxT, params, redirect, renderRoute)
import Web.Spock.Core
       (ClientPreferredFormat(PrefJSON), SpockCtxT, get, json, jsonBody',
        preferredFormat, post)

import Kucipong.Db (Coupon(..), CouponType(..), Image(..), Key(..))
import Kucipong.Form
       (StoreNewCouponForm(..), StoreSetImageForm(..),
        removeNonUsedCouponInfo)
import Kucipong.Handler.Error (resp404)
import Kucipong.Handler.Route
       (storeCouponR, storeCouponCreateR, storeCouponDeleteVarR,
        storeCouponVarR, storeCouponVarEditR, storeCouponVarSetImageR,
        storeR)
import Kucipong.Handler.Store.TemplatePath
       (templateCoupon, templateCouponCreate, templateCouponDelete,
        templateCouponId, templateCouponIdEdit)
import Kucipong.Handler.Store.Types
       (StoreError(..), CouponView(..), CouponViewImageUrl(..),
        CouponViewKey(..), CouponViewText(..), CouponViewTexts(..),
        CouponViewCouponType(..), StoreViewText(..))
import Kucipong.Handler.Store.Util
       (awsUrlFromMaybeImageKey, guardMaybeImageKeyOwnedByStore)
import Kucipong.Handler.Types (PageViewer(..))
import Kucipong.I18n (label)
import Kucipong.Monad
       (CouponDeleteResult(..), MonadKucipongAws(..), MonadKucipongDb(..),
        awsGetBucketName, awsUrlFromImageAndBucket,
        dbFindCouponByStoreKeyAndCouponKey, dbFindCouponsByStoreKey,
        dbFindImagesForCoupons, dbFindStoreByStoreKey, dbInsertCoupon,
        dbUpdateCoupon, dbUpdateCouponImage)
import Kucipong.RenderTemplate (renderTemplateFromEnv)
import Kucipong.Session (Session, SessionType(SessionTypeStore))
import Kucipong.Spock
       (pattern StoreSession, ContainsStoreSession, getReqParamErr,
        getStoreKey, jsonErrorStatus, jsonSuccess)

couponNewGet
  :: forall xs m.
     (MonadIO m)
  => ActionCtxT (HVect xs) m ()
couponNewGet = do
  let action = renderRoute storeCouponCreateR
      coupon = mempty :: [(Text, Text)]
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
  couponEntity <-
    fromMaybeM (resp404 [label def StoreErrorCouponNotFound]) maybeCouponEntity
  maybeStoreEntity <- dbFindStoreByStoreKey storeKey
  storeEntity <-
    fromMaybeM (resp404 [label def StoreErrorNoStore]) maybeStoreEntity
  let maybeImageKey = couponImage $ entityVal couponEntity
  maybeImageUrl <- awsUrlFromMaybeImageKey maybeImageKey
  let coupon = CouponView storeEntity couponEntity maybeImageUrl
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
  coupon <-
    fromMaybeM (resp404 [label def StoreErrorCouponNotFound]) maybeCouponEntity
  let action = renderRoute storeCouponVarEditR couponKey
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
      (view _Wrapped imageKey)
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
      coupon <- params
      $(logDebug) $ "params: " <> tshow coupon
      $(logDebug) $
        "got following error in store couponEditPost handler: " <> errMsg
      let errors = [errMsg]
          action = renderRoute storeCouponVarEditR couponKey
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
  couponEntitiesAndImages <- dbFindImagesForCoupons couponEntities
  let awsImageUrlFunc =
        fmap $ awsUrlFromImageAndBucket bucketName . imageS3Name . entityVal
  $(renderTemplateFromEnv templateCoupon)


couponPost
  :: forall xs n m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongDb m
     , MonadLogger m
     )
  => ActionCtxT (HVect xs) m ()
couponPost = do
  (StoreSession storeKey) <- getStoreKey
  storeNewCouponForm <- getReqParamErr handleErr
  let StoreNewCouponForm {..} = removeNonUsedCouponInfo storeNewCouponForm
  void $
    dbInsertCoupon
      storeKey
      title
      couponType
      (view _Wrapped validFrom)
      (view _Wrapped validUntil)
      (view _Wrapped imageKey)
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
    handleErr :: Text -> ActionCtxT (HVect xs) m a
    handleErr errMsg = do
      coupon <- params
      $(logDebug) $ "params: " <> tshow coupon
      $(logDebug) $ "got following error in store couponPost handler: " <> errMsg
      let errors = [errMsg]
          action = renderRoute storeCouponCreateR
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

couponSetImagePost
  :: forall n xs m.
     ( ContainsStoreSession n xs
     , MonadIO m
     , MonadKucipongDb m
     )
  => Key Coupon -> ActionCtxT (HVect xs) m ()
couponSetImagePost couponKey = do
  (StoreSession storeKey) <- getStoreKey
  StoreSetImageForm maybeImageKey <- jsonBody'
  guardMaybeImageKeyOwnedByStore storeKey maybeImageKey handleErr
  void $ dbUpdateCouponImage couponKey storeKey maybeImageKey
  jsonSuccess maybeImageKey
  where
    handleErr :: ActionCtxT (HVect xs) m a
    handleErr =
      jsonErrorStatus
        badRequest400
        StoreErrorImageOwnedByStore
        (label def StoreErrorImageOwnedByStore)

allCouponTypes :: [CouponType]
allCouponTypes = [minBound .. maxBound]

storeCouponComponent
  :: forall m xs.
     ( MonadIO m
     , MonadKucipongAws m
     , MonadKucipongDb m
     , MonadLogger m
     )
  => SpockCtxT (HVect (Session 'SessionTypeStore : xs)) m ()
storeCouponComponent = do
  get storeCouponR couponListGet
  post storeCouponCreateR couponPost
  get storeCouponCreateR couponNewGet
  get storeCouponDeleteVarR couponDeleteGet
  post storeCouponDeleteVarR couponDeletePost
  get storeCouponVarR couponGet
  get storeCouponVarEditR couponEditGet
  post storeCouponVarEditR couponEditPost
  post storeCouponVarSetImageR couponSetImagePost

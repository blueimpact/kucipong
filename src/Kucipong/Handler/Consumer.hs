{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Consumer
  ( module Kucipong.Handler.Consumer
  ) where

import Kucipong.Prelude

import Control.FromSum (fromMaybeM)
import Data.Default (def)
import Database.Persist.Sql (Entity(..))
import Web.Spock (ActionCtxT, renderRoute)
import Web.Spock.Core (SpockCtxT, get)

import Kucipong.Db
       (Coupon(..), CouponType(..), Image(..), Key(..), Store(..))
import Kucipong.Handler.Consumer.TemplatePath
import Kucipong.Handler.Consumer.Types (ConsumerError(..))
import Kucipong.Handler.Error (resp404)
import Kucipong.Handler.Route
       (consumerCouponVarR, consumerStoreVarR, consumerStoreVarCouponR)
import Kucipong.Handler.Store.Types
       (CouponViewCouponType(..),
        CouponViewText(..), CouponViewTexts(..),
        CouponViewCouponType(..), StoreError(..),
        StoreViewText(..), StoreViewTexts(..))
import Kucipong.I18n (label)
import Kucipong.Monad
       (MonadKucipongAws(..), MonadKucipongDb(..), awsImageS3Url,
        dbFindImage, dbFindPublicCouponById, dbFindStoreByStoreKey)
import Kucipong.RenderTemplate (renderTemplateFromEnv)
import Kucipong.View.Instance (ImageUrl(..))

couponGet
  :: forall ctx m.
     (MonadIO m, MonadKucipongAws m, MonadKucipongDb m)
  => Key Coupon -> ActionCtxT ctx m ()
couponGet couponKey = do
  maybeCouponEntity <- dbFindPublicCouponById couponKey
  coupon <-
    fromMaybeM (handleErr $ label def ConsumerErrorCouldNotFindCoupon) maybeCouponEntity
  maybeStoreEntity <- dbFindStoreByStoreKey . couponStoreId . entityVal $ coupon
  store <-
    fromMaybeM (resp404 [label def StoreErrorNoStore]) maybeStoreEntity
  imageUrl <- imageUrlFromImageKey . couponImage $ entityVal coupon
  let
    aboutStore =
      maybe mempty
        (renderRoute consumerStoreVarR . entityKey)
        maybeStoreEntity
  $(renderTemplateFromEnv templateCouponId)
  where
    handleErr :: Text -> ActionCtxT ctx m a
    handleErr errMsg = do
      let errors = [errMsg]
      $(renderTemplateFromEnv templateCategory)

storeGet
  :: forall ctx m.
     (MonadIO m, MonadKucipongAws m, MonadKucipongDb m)
  => Key Store -> ActionCtxT ctx m ()
storeGet storeKey = do
  maybeStoreEntity <- dbFindStoreByStoreKey storeKey
  storeEntity <-
    fromMaybeM
      (handleErr $ label def ConsumerErrorCouldNotFindStore)
      maybeStoreEntity
  imageUrl <- imageUrlFromImageKey . storeImage $ entityVal storeEntity
  store <-
    fromMaybeM (resp404 [label def StoreErrorNoStore]) maybeStoreEntity
  $(renderTemplateFromEnv templateStoreId)
  where
    handleErr :: Text -> ActionCtxT ctx m a
    handleErr errMsg = do
      let errors = [errMsg]
      $(renderTemplateFromEnv templateCategory)

imageUrlFromImageKey
  :: (MonadKucipongAws m, MonadKucipongDb m)
  => Maybe (Key Image) -> m (Maybe Text)
imageUrlFromImageKey maybeImageKey = do
  maybeImageEntity <- join <$> traverse dbFindImage maybeImageKey
  let maybeImageName = imageS3Name . entityVal <$> maybeImageEntity
  traverse awsImageS3Url maybeImageName

consumerComponent
  :: forall m.
     (MonadIO m, MonadKucipongAws m, MonadKucipongDb m)
  => SpockCtxT () m ()
consumerComponent = do
  get consumerCouponVarR couponGet
  get consumerStoreVarR storeGet
  get consumerStoreVarCouponR undefined

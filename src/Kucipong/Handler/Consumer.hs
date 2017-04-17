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

import Kucipong.Db (Coupon(..), CouponType(..), Key(..))
import Kucipong.Handler.Consumer.TemplatePath
import Kucipong.Handler.Consumer.Types (ConsumerError(..))
import Kucipong.Handler.Error (resp404)
import Kucipong.Handler.Route
       (consumerCouponVarR, consumerStoreVarR)
import Kucipong.Handler.Store.Types
       (CouponViewText(..), CouponViewTexts(..), CouponViewCouponType(..),
        StoreError(..), StoreViewText(..))
import Kucipong.Handler.Store.Util (awsUrlFromMaybeImageKey)
import Kucipong.I18n (label)
import Kucipong.Monad
       (MonadKucipongAws(..), MonadKucipongDb(..), dbFindPublicCouponById,
        dbFindStoreByStoreKey)
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
  let maybeImageKey = couponImage . entityVal =<< maybeCouponEntity
  imageUrl <- awsUrlFromMaybeImageKey maybeImageKey
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

consumerComponent
  :: forall m.
     (MonadIO m, MonadKucipongAws m, MonadKucipongDb m)
  => SpockCtxT () m ()
consumerComponent = get consumerCouponVarR couponGet

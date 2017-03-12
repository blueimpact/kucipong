{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Consumer
  ( module Kucipong.Handler.Consumer
  ) where

import Kucipong.Prelude

import Control.FromSum (fromMaybeM)
import Data.Default (def)
import Database.Persist.Sql (Entity(..), fromSqlKey)
import Web.Spock (ActionCtxT, renderRoute)
import Web.Spock.Core (SpockCtxT, get)

import Kucipong.Db (Coupon(..), CouponType(..), Key(..), Store(..))
import Kucipong.Handler.Consumer.Types (ConsumerError(..))
import Kucipong.Handler.Route (consumerCouponVarR, storeR)
import Kucipong.I18n (label)
import Kucipong.Monad
       (MonadKucipongAws(..), MonadKucipongDb(..), awsImageS3Url,
        dbFindByKey, dbFindCouponById)
import Kucipong.RenderTemplate (renderTemplateFromEnv)

couponGet
  :: forall ctx m.
     (MonadIO m, MonadKucipongAws m, MonadKucipongDb m)
  => Key Coupon -> ActionCtxT ctx m ()
couponGet couponKey = do
  maybeCouponEntity <- dbFindCouponById couponKey
  Entity _ coupon <-
    fromMaybeM (handleErr $ label def ConsumerErrorCouldNotFindCoupon) maybeCouponEntity
  maybeStoreEntity <- dbFindByKey (couponStoreEmail coupon)
  let maybeImage = couponImage . entityVal =<< maybeCouponEntity
  maybeImageUrl <- traverse awsImageS3Url maybeImage
  -- TODO
  -- let storeR = undefined
  $(renderTemplateFromEnv "endUser_coupon_id.html")
  where
    handleErr :: Text -> ActionCtxT ctx m a
    handleErr errMsg = undefined

consumerComponent
  :: forall m.
     (MonadIO m, MonadKucipongAws m, MonadKucipongDb m)
  => SpockCtxT () m ()
consumerComponent = get consumerCouponVarR couponGet

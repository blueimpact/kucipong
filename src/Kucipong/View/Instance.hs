{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.View.Instance where

import Kucipong.Prelude

import Database.Persist (Entity(..))
import Database.Persist.Sql (fromSqlKey)

import Kucipong.Db
       (Coupon(..), CouponType(..), Percent, Price, Store(..),
        percentToText, priceToText)
import Kucipong.Handler.Store.Types
       (CouponView(..), CouponViewKey(..), CouponViewTypes(..),
        CouponViewConditions(..), CouponViewCouponType(..))
import Kucipong.View.Class (View(..))

instance View CouponView CouponViewKey where
  type ViewO CouponViewKey = Int64
  format StoreId = fromSqlKey . entityKey . couponStore
  format CouponId = fromSqlKey . entityKey . couponCoupon

instance View CouponView CouponViewTypes where
  type ViewO CouponViewTypes = Text
  format StoreName = fromMaybe "(no title)" . storeName . store
  format StoreAddress = fromMaybe mempty . storeAddress . store
  format ImageUrl = fromMaybe mempty . couponImageUrl
  format Title = couponTitle . coupon
  format ValidFrom = maybe mempty formatValidFrom . couponValidFrom . coupon
  format ValidUntil = maybe mempty formatValidUntil . couponValidFrom . coupon
  format DiscountPercent =
    maybe mempty formatDiscountPercent . couponDiscountPercent . coupon
  format DiscountMinimumPrice =
    maybe mempty formatCurrency . couponDiscountMinimumPrice . coupon
  format GiftContent = maybe mempty tshow . couponGiftContent . coupon
  format GiftMinimumPrice =
    maybe mempty formatCurrency . couponGiftMinimumPrice . coupon
  format GiftReferencePrice =
    maybe mempty formatCurrency . couponGiftReferencePrice . coupon
  format SetContent = maybe mempty tshow . couponSetContent . coupon
  format SetPrice = maybe mempty formatCurrency . couponSetPrice . coupon
  format SetReferencePrice =
    maybe mempty formatCurrency . couponSetReferencePrice . coupon
  format OtherContent = maybe mempty tshow . couponOtherContent . coupon

instance View CouponView CouponViewConditions where
  type ViewO CouponViewConditions = [Text]
  format DiscountOtherConditions =
    concatMap lines . couponDiscountOtherConditions . coupon
  format GiftOtherConditions =
    concatMap lines . couponGiftOtherConditions . coupon
  format SetOtherConditions =
    concatMap lines . couponSetOtherConditions . coupon
  format OtherConditions = concatMap lines . couponOtherConditions . coupon

instance View CouponView CouponViewCouponType where
  type ViewO CouponViewCouponType = CouponType
  format CouponType = couponCouponType . coupon

store :: CouponView -> Store
store = entityVal . couponStore

coupon :: CouponView -> Coupon
coupon = entityVal . couponCoupon

-- Helper functions
formatValidFrom :: Day -> Text
formatValidFrom day = "From " <> tshow day

formatValidUntil :: Day -> Text
formatValidUntil day = "To " <> tshow day

formatCurrency :: Price -> Text
formatCurrency p = "$" <> priceToText p

formatDiscountPercent :: Percent -> Text
formatDiscountPercent p = percentToText p <> "%"

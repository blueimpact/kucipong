{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.View.Instance where

import Kucipong.Prelude

import Database.Persist (Entity(..))
import Database.Persist.Sql (fromSqlKey)

import Kucipong.Db
       (BusinessCategory, BusinessCategoryDetail, Coupon(..),
        CouponType(..), Percent, Price, Store(..), percentToText,
        priceToText)
import Kucipong.Handler.Store.Types
       (CouponView(..), CouponViewImageUrl(..), CouponViewKey(..),
        CouponViewTypes(..), CouponViewConditions(..),
        CouponViewCouponType(..), StoreView(..),
        StoreViewBusinessCategory(..),
        StoreViewBusinessCategoryDetails(..), StoreViewDefaultImage(..),
        StoreViewImageUrl(..), StoreViewText(..), StoreViewTexts(..))
import Kucipong.View.Class (View(..))

instance View CouponView CouponViewKey where
  type ViewO CouponViewKey = Int64
  format StoreId = fromSqlKey . entityKey . couponStore
  format CouponId = fromSqlKey . entityKey . couponCoupon

instance View CouponView CouponViewTypes where
  type ViewO CouponViewTypes = Text
  format a = format a . entityVal . couponCoupon

instance View [(Text, Text)] CouponViewTypes where
  type ViewO CouponViewTypes = Text
  format Title = fromMaybe mempty . lookup "title"
  format ValidFrom = fromMaybe mempty . lookup "validFrom"
  format ValidUntil = fromMaybe mempty . lookup "validUntil"
  format DiscountPercent = fromMaybe mempty . lookup "discountPercent"
  format DiscountMinimumPrice = fromMaybe mempty . lookup "discountMinimumPrice"
  format GiftContent = fromMaybe mempty . lookup "giftContent"
  format GiftMinimumPrice = fromMaybe mempty . lookup "giftMinimumPrice"
  format GiftReferencePrice = fromMaybe mempty . lookup "giftReferencePrice"
  format SetContent = fromMaybe mempty . lookup "setContent"
  format SetPrice = fromMaybe mempty . lookup "setPrice"
  format SetReferencePrice = fromMaybe mempty . lookup "setReferencePrice"
  format OtherContent = fromMaybe mempty . lookup "otherContent"

instance View CouponView CouponViewConditions where
  type ViewO CouponViewConditions = [Text]
  format a = format a . entityVal . couponCoupon

instance View [(Text, Text)] CouponViewConditions where
  type ViewO CouponViewConditions = [Text]
  format DiscountOtherConditions =
    fromMaybe mempty . (lines <$>) . lookup "discountOtherConditions"
  format GiftOtherConditions =
    fromMaybe mempty . (lines <$>) . lookup "giftOtherConditions"
  format SetOtherConditions =
    fromMaybe mempty . (lines <$>) . lookup "setOtherConditions"
  format OtherConditions =
    fromMaybe mempty . (lines <$>) . lookup "otherConditions"

instance View CouponView CouponViewCouponType where
  type ViewO CouponViewCouponType = CouponType
  format a = format a . entityVal . couponCoupon

instance View [(Text, Text)] CouponViewCouponType where
  type ViewO CouponViewCouponType = CouponType
  format CouponType = fromMaybe minBound . (readMay =<<) . lookup "couponType"

instance View CouponView CouponViewImageUrl where
  type ViewO CouponViewImageUrl = Text
  format CouponImageUrl = fromMaybe mempty . couponImageUrl

instance View CouponView StoreViewText where
  type ViewO StoreViewText = Text
  format a = format a . entityVal . couponStore

-- ---------------
--  Store coupon
-- ---------------

instance View Coupon CouponViewTypes where
  type ViewO CouponViewTypes = Text
  format Title = couponTitle
  format ValidFrom = maybe mempty formatValidFrom . couponValidFrom
  format ValidUntil = maybe mempty formatValidUntil . couponValidFrom
  format DiscountPercent =
    maybe mempty formatDiscountPercent . couponDiscountPercent
  format DiscountMinimumPrice =
    maybe mempty formatCurrency . couponDiscountMinimumPrice
  format GiftContent = maybe mempty tshow . couponGiftContent
  format GiftMinimumPrice =
    maybe mempty formatCurrency . couponGiftMinimumPrice
  format GiftReferencePrice =
    maybe mempty formatCurrency . couponGiftReferencePrice
  format SetContent = maybe mempty tshow . couponSetContent
  format SetPrice = maybe mempty formatCurrency . couponSetPrice
  format SetReferencePrice =
    maybe mempty formatCurrency . couponSetReferencePrice
  format OtherContent = maybe mempty tshow . couponOtherContent

instance View Coupon CouponViewConditions where
  type ViewO CouponViewConditions = [Text]
  format DiscountOtherConditions =
    concatMap lines . couponDiscountOtherConditions
  format GiftOtherConditions =
    concatMap lines . couponGiftOtherConditions
  format SetOtherConditions =
    concatMap lines . couponSetOtherConditions
  format OtherConditions = concatMap lines . couponOtherConditions

instance View Coupon CouponViewCouponType where
  type ViewO CouponViewCouponType = CouponType
  format CouponType = couponCouponType

-- -------
--  Store
-- -------

instance View Store StoreViewText where
  type ViewO StoreViewText = Text
  format StoreName = fromMaybe "(no store name)" . storeName
  format StoreSalesPoint = fromMaybe mempty . storeSalesPoint
  format StoreAddress = fromMaybe mempty . storeAddress
  format StorePhoneNumber = fromMaybe mempty . storePhoneNumber
  format StoreRegularHoliday = fromMaybe mempty . storeRegularHoliday
  format StoreUrl = fromMaybe mempty . storeUrl

instance View Store StoreViewTexts where
  type ViewO StoreViewTexts = [Text]
  format StoreBusinessHour =
    concatMap lines . storeBusinessHours

instance View StoreView StoreViewImageUrl where
  type ViewO StoreViewImageUrl = Text
  format StoreImageUrl = fromMaybe mempty . storeImageUrl

instance View StoreView StoreViewDefaultImage where
  type ViewO StoreViewDefaultImage = Text
  format StoreDefaultImage = fromMaybe mempty . storeDefaultImage

instance View Store StoreViewBusinessCategory where
  type ViewO StoreViewBusinessCategory = BusinessCategory
  format StoreBusinessCategory = fromMaybe minBound . storeBusinessCategory

instance View Store StoreViewBusinessCategoryDetails where
  type ViewO StoreViewBusinessCategoryDetails = [BusinessCategoryDetail]
  format StoreBusinessCategoryDetails = storeBusinessCategoryDetails

instance View StoreView StoreViewBusinessCategory where
  type ViewO StoreViewBusinessCategory = BusinessCategory
  format a = format a . entityVal . storeEntity

instance View StoreView StoreViewBusinessCategoryDetails where
  type ViewO StoreViewBusinessCategoryDetails = [BusinessCategoryDetail]
  format a = format a . entityVal . storeEntity

instance View StoreView StoreViewText where
  type ViewO StoreViewText = Text
  format a = format a . entityVal . storeEntity

instance View StoreView StoreViewTexts where
  type ViewO StoreViewTexts = [Text]
  format a = format a . entityVal . storeEntity

-- Helper functions
formatValidFrom :: Day -> Text
formatValidFrom day = "From " <> tshow day

formatValidUntil :: Day -> Text
formatValidUntil day = "To " <> tshow day

formatCurrency :: Price -> Text
formatCurrency p = "$" <> priceToText p

formatDiscountPercent :: Percent -> Text
formatDiscountPercent p = percentToText p <> "%"

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
import Kucipong.View.Class (View(..), ViewO)

type instance ViewO CouponViewConditions = [Text]
type instance ViewO CouponViewCouponType = CouponType
type instance ViewO CouponViewImageUrl = Text
type instance ViewO CouponViewKey = Int64
type instance ViewO CouponViewTypes = Text
type instance ViewO StoreViewBusinessCategory = BusinessCategory
type instance ViewO StoreViewBusinessCategoryDetails = [BusinessCategoryDetail]
type instance ViewO StoreViewDefaultImage = Text
type instance ViewO StoreViewImageUrl = Text
type instance ViewO StoreViewText = Text
type instance ViewO StoreViewTexts = [Text]

instance (View v a) => View (Entity v) a where
  format :: a -> Entity v -> ViewO a
  format a (Entity _ v) = format a v

instance View CouponView CouponViewKey where
  format StoreId = fromSqlKey . entityKey . couponStore
  format CouponId = fromSqlKey . entityKey . couponCoupon

instance View CouponView CouponViewTypes where
  format a = format a . entityVal . couponCoupon

instance View [(Text, Text)] CouponViewTypes where
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
  format a = format a . entityVal . couponCoupon

instance View [(Text, Text)] CouponViewConditions where
  format DiscountOtherConditions =
    fromMaybe mempty . (lines <$>) . lookup "discountOtherConditions"
  format GiftOtherConditions =
    fromMaybe mempty . (lines <$>) . lookup "giftOtherConditions"
  format SetOtherConditions =
    fromMaybe mempty . (lines <$>) . lookup "setOtherConditions"
  format OtherConditions =
    fromMaybe mempty . (lines <$>) . lookup "otherConditions"

instance View CouponView CouponViewCouponType where
  format a = format a . entityVal . couponCoupon

instance View [(Text, Text)] CouponViewCouponType where
  format CouponType = fromMaybe minBound . (readMay =<<) . lookup "couponType"

instance View CouponView CouponViewImageUrl where
  format CouponImageUrl = fromMaybe mempty . couponImageUrl

instance View CouponView StoreViewText where
  format a = format a . entityVal . couponStore

-- ---------------
--  Store coupon
-- ---------------

instance View Coupon CouponViewTypes where
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
  format DiscountOtherConditions =
    concatMap lines . couponDiscountOtherConditions
  format GiftOtherConditions =
    concatMap lines . couponGiftOtherConditions
  format SetOtherConditions =
    concatMap lines . couponSetOtherConditions
  format OtherConditions = concatMap lines . couponOtherConditions

instance View Coupon CouponViewCouponType where
  format CouponType = couponCouponType

-- -------
--  Store
-- -------

instance View Store StoreViewText where
  format StoreName = fromMaybe "(no store name)" . storeName
  format StoreSalesPoint = fromMaybe mempty . storeSalesPoint
  format StoreAddress = fromMaybe mempty . storeAddress
  format StorePhoneNumber = fromMaybe mempty . storePhoneNumber
  format StoreRegularHoliday = fromMaybe mempty . storeRegularHoliday
  format StoreUrl = fromMaybe mempty . storeUrl

instance View Store StoreViewTexts where
  format StoreBusinessHour =
    concatMap lines . storeBusinessHours

instance View StoreView StoreViewImageUrl where
  format StoreImageUrl = fromMaybe mempty . storeImageUrl

instance View StoreView StoreViewDefaultImage where
  format StoreDefaultImage = fromMaybe mempty . storeDefaultImage

instance View Store StoreViewBusinessCategory where
  format StoreBusinessCategory = fromMaybe minBound . storeBusinessCategory

instance View Store StoreViewBusinessCategoryDetails where
  format StoreBusinessCategoryDetails = storeBusinessCategoryDetails

instance View StoreView StoreViewBusinessCategory where
  format a = format a . entityVal . storeEntity

instance View StoreView StoreViewBusinessCategoryDetails where
  format a = format a . entityVal . storeEntity

instance View StoreView StoreViewText where
  format a = format a . entityVal . storeEntity

instance View StoreView StoreViewTexts where
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

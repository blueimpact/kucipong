{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.View.Instance where

import Kucipong.Prelude

import Data.Default (Default, def)
import Database.Persist (Entity(..))
import Database.Persist.Sql (fromSqlKey)
import Web.HttpApiData (FromHttpApiData, parseQueryParamMaybe)

import Kucipong.Db
       (BusinessCategory, BusinessCategoryDetail, Coupon(..),
        CouponType(..), Percent, Price, Store(..), percentToText,
        priceToText)
import Kucipong.Handler.Store.Types
       (CouponView(..), CouponViewImageUrl(..), CouponViewKey(..),
        CouponViewText(..), CouponViewTexts(..),
        CouponViewCouponType(..), StoreView(..),
        StoreViewBusinessCategory(..),
        StoreViewBusinessCategoryDetails(..), StoreViewImageUrl(..),
        StoreViewText(..), StoreViewTexts(..))
import Kucipong.View.Class (ToName(..), View(..), ViewO)

-- --------
--  ToName
-- --------

instance ToName StoreViewText where
  toName StoreName = "name"
  toName StoreSalesPoint = "salesPoint"
  toName StoreAddress = "address"
  toName StorePhoneNumber = "phoneNumber"
  toName StoreRegularHoliday = "regularHoliday"
  toName StoreUrl = "url"

instance ToName StoreViewTexts where
  toName StoreBusinessHour = "businessHours"

instance ToName StoreViewBusinessCategory where
  toName StoreBusinessCategory = "businessCategory"

instance ToName StoreViewBusinessCategoryDetails where
  toName StoreBusinessCategoryDetails = "businessCategoryDetails"

instance ToName CouponViewText where
  toName CouponTitle = "title"
  toName CouponValidFrom = "validFrom"
  toName CouponValidUntil = "validUntil"
  toName CouponDiscountPercent = "discountPercent"
  toName CouponDiscountMinimumPrice = "discountMinimumPrice"
  toName CouponGiftContent = "giftContent"
  toName CouponGiftMinimumPrice = "giftMinimumPrice"
  toName CouponGiftReferencePrice = "giftReferencePrice"
  toName CouponSetContent = "setContent"
  toName CouponSetPrice = "setPrice"
  toName CouponSetReferencePrice = "setReferencePrice"
  toName CouponOtherContent = "otherContent"

instance ToName CouponViewTexts where
  toName CouponDiscountOtherConditions = "discountOtherConditions"
  toName CouponGiftOtherConditions = "giftOtherConditions"
  toName CouponSetOtherConditions = "setOtherConditions"
  toName CouponOtherConditions = "otherConditions"

instance ToName CouponViewCouponType where
  toName CouponCouponType = "couponType"

-- ------
--  View
-- ------

type instance ViewO CouponViewTexts = [Text]
type instance ViewO CouponViewCouponType = CouponType
type instance ViewO CouponViewImageUrl = Text
type instance ViewO CouponViewKey = Int64
type instance ViewO CouponViewText = Text
type instance ViewO StoreViewBusinessCategory = BusinessCategory
type instance ViewO StoreViewBusinessCategoryDetails = [BusinessCategoryDetail]
type instance ViewO StoreViewImageUrl = Text
type instance ViewO StoreViewText = Text
type instance ViewO StoreViewTexts = [Text]

instance (View v a) => View (Entity v) a where
  format :: a -> Entity v -> ViewO a
  format a (Entity _ v) = format a v

instance (Default (ViewO t), FromHttpApiData (ViewO t), ToName t) =>
         View [(Text, Text)] t where
  format a = fromMaybe def . (parseQueryParamMaybe =<<) . lookup (toName a)

instance View CouponView CouponViewKey where
  format StoreId = fromSqlKey . entityKey . couponStore
  format CouponId = fromSqlKey . entityKey . couponCoupon

instance View CouponView CouponViewText where
  format a = format a . entityVal . couponCoupon

instance View CouponView CouponViewTexts where
  format a = format a . entityVal . couponCoupon

instance View CouponView CouponViewCouponType where
  format a = format a . entityVal . couponCoupon

instance View CouponView CouponViewImageUrl where
  format CouponImageUrl = fromMaybe mempty . couponImageUrl

instance View CouponView StoreViewText where
  format a = format a . entityVal . couponStore

-- ---------------
--  Store coupon
-- ---------------

instance View Coupon CouponViewText where
  format CouponTitle = couponTitle
  format CouponValidFrom = maybe mempty formatValidFrom . couponValidFrom
  format CouponValidUntil = maybe mempty formatValidUntil . couponValidFrom
  format CouponDiscountPercent =
    maybe mempty formatDiscountPercent . couponDiscountPercent
  format CouponDiscountMinimumPrice =
    maybe mempty formatCurrency . couponDiscountMinimumPrice
  format CouponGiftContent = maybe mempty tshow . couponGiftContent
  format CouponGiftMinimumPrice =
    maybe mempty formatCurrency . couponGiftMinimumPrice
  format CouponGiftReferencePrice =
    maybe mempty formatCurrency . couponGiftReferencePrice
  format CouponSetContent = maybe mempty tshow . couponSetContent
  format CouponSetPrice = maybe mempty formatCurrency . couponSetPrice
  format CouponSetReferencePrice =
    maybe mempty formatCurrency . couponSetReferencePrice
  format CouponOtherContent = maybe mempty tshow . couponOtherContent

instance View Coupon CouponViewTexts where
  format CouponDiscountOtherConditions =
    concatMap lines . couponDiscountOtherConditions
  format CouponGiftOtherConditions =
    concatMap lines . couponGiftOtherConditions
  format CouponSetOtherConditions =
    concatMap lines . couponSetOtherConditions
  format CouponOtherConditions = concatMap lines . couponOtherConditions

instance View Coupon CouponViewCouponType where
  format CouponCouponType = couponCouponType

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

instance View StoreView StoreViewText where
  format a = format a . entityVal . storeEntity

instance View Store StoreViewTexts where
  format StoreBusinessHour =
    concatMap lines . storeBusinessHours

instance View StoreView StoreViewTexts where
  format a = format a . entityVal . storeEntity

instance View StoreView StoreViewImageUrl where
  format StoreImageUrl = fromMaybe mempty . storeImageUrl

instance View Store StoreViewBusinessCategory where
  format StoreBusinessCategory = fromMaybe minBound . storeBusinessCategory

instance View StoreView StoreViewBusinessCategory where
  format a = format a . entityVal . storeEntity

instance View Store StoreViewBusinessCategoryDetails where
  format StoreBusinessCategoryDetails = storeBusinessCategoryDetails

instance View StoreView StoreViewBusinessCategoryDetails where
  format a = format a . entityVal . storeEntity

-- ------------------
--  Helper functions
-- ------------------

formatValidFrom :: Day -> Text
formatValidFrom day = "From " <> tshow day

formatValidUntil :: Day -> Text
formatValidUntil day = "To " <> tshow day

formatCurrency :: Price -> Text
formatCurrency p = "$" <> priceToText p

formatDiscountPercent :: Percent -> Text
formatDiscountPercent p = percentToText p <> "%"


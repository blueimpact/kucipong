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
        CouponViewTypes(..), CouponViewConditions(..),
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

instance ToName CouponViewTypes where
  toName Title = "title"
  toName ValidFrom = "validFrom"
  toName ValidUntil = "validUntil"
  toName DiscountPercent = "discountPercent"
  toName DiscountMinimumPrice = "discountMinimumPrice"
  toName GiftContent = "giftContent"
  toName GiftMinimumPrice = "giftMinimumPrice"
  toName GiftReferencePrice = "giftReferencePrice"
  toName SetContent = "setContent"
  toName SetPrice = "setPrice"
  toName SetReferencePrice = "setReferencePrice"
  toName OtherContent = "otherContent"

instance ToName CouponViewConditions where
  toName DiscountOtherConditions = "discountOtherConditions"
  toName GiftOtherConditions = "giftOtherConditions"
  toName SetOtherConditions = "setOtherConditions"
  toName OtherConditions = "otherConditions"

instance ToName CouponViewCouponType where
  toName CouponType = "couponType"

-- ------
--  View
-- ------

type instance ViewO CouponViewConditions = [Text]
type instance ViewO CouponViewCouponType = CouponType
type instance ViewO CouponViewImageUrl = Text
type instance ViewO CouponViewKey = Int64
type instance ViewO CouponViewTypes = Text
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

instance View CouponView CouponViewTypes where
  format a = format a . entityVal . couponCoupon

instance View CouponView CouponViewConditions where
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


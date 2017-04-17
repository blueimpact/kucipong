{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.View.Instance where

import Kucipong.Prelude

import Data.Default (Default, def)
import Database.Persist (Entity(..), ToBackendKey)
import Database.Persist.Sql (SqlBackend, fromSqlKey)
import Web.HttpApiData (FromHttpApiData, parseQueryParamMaybe)

import Kucipong.Db
       (BusinessCategory(..), BusinessCategoryDetail(..), Coupon(..),
        CouponType(..), Percent, Price, Store(..), percentToText,
        priceToText)
import Kucipong.Handler.Store.Types
       (CouponViewText(..), CouponViewTexts(..), CouponViewCouponType(..),
        StoreViewBusinessCategory(..),
        StoreViewBusinessCategoryDetails(..), StoreViewText(..),
        StoreViewTexts(..))
import Kucipong.View.Class
       (ToKey(..), ToKeyO, ToName(..), View(..), ViewO)

data VKey = VKey
data ImageUrl = ImageUrl

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

-- -------
--  ToKey
-- -------

type instance ToKeyO (Entity v) = Int64
type instance ToKeyO BusinessCategory = Text
type instance ToKeyO BusinessCategoryDetail = Text
type instance ToKeyO CouponType = Text

instance (ToBackendKey SqlBackend v) => ToKey (Entity v) where
  key = fromSqlKey . entityKey

instance ToKey BusinessCategory
instance ToKey BusinessCategoryDetail
instance ToKey CouponType

-- ------
--  View
-- ------

type instance ViewO VKey = Int64
type instance ViewO ImageUrl = Maybe Text
type instance ViewO CouponViewTexts = [Text]
type instance ViewO CouponViewCouponType = CouponType
type instance ViewO CouponViewText = Text
type instance ViewO StoreViewBusinessCategory = BusinessCategory
type instance ViewO StoreViewBusinessCategoryDetails = [BusinessCategoryDetail]
type instance ViewO StoreViewText = Text
type instance ViewO StoreViewTexts = [Text]

instance (View v a) => View (Entity v) a where
  format :: a -> Entity v -> ViewO a
  format a (Entity _ v) = format a v

instance (ToBackendKey SqlBackend v) => View (Entity v) VKey where
  format :: VKey -> Entity v -> ViewO VKey
  format VKey = fromSqlKey . entityKey

instance View (Maybe Text) ImageUrl where
  format :: ImageUrl -> Maybe Text -> ViewO ImageUrl
  format ImageUrl = id

instance (Default (ViewO t), FromHttpApiData (ViewO t), ToName t) =>
         View [(Text, Text)] t where
  format a = fromMaybe def . (parseQueryParamMaybe =<<) . lookup (toName a)

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

instance View Store StoreViewTexts where
  format StoreBusinessHour =
    concatMap lines . storeBusinessHours

instance View Store StoreViewBusinessCategory where
  format StoreBusinessCategory = fromMaybe minBound . storeBusinessCategory

instance View Store StoreViewBusinessCategoryDetails where
  format StoreBusinessCategoryDetails = storeBusinessCategoryDetails

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


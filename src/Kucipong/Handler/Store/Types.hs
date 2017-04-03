module Kucipong.Handler.Store.Types
  ( StoreError(..)
  , StoreMsg(..)
  , CouponView(..)
  , CouponViewKey(..)
  , CouponViewTypes(..)
  , CouponViewConditions(..)
  , CouponViewCouponType(..)
  , CouponViewImageUrl(..)
  , StoreView(..)
  , StoreViewImageUrl(..)
  , StoreViewBusinessCategory(..)
  , StoreViewBusinessCategoryDetails(..)
  , StoreViewText(..)
  , StoreViewTexts(..)
  ) where

import Kucipong.Prelude

import Database.Persist (Entity(..))

import Kucipong.Db (Coupon(..), Store(..))

-- For I18n.
data StoreError
  = StoreErrorBusinessCategoryDetailIncorrect
  | StoreErrorCouldNotSendEmail
  | StoreErrorCouldNotUploadImage
  | StoreErrorCouponNotFound
  | StoreErrorNoStore
  | StoreErrorNoStoreEmail EmailAddress
  | StoreErrorNotAnImage
  deriving (Show, Eq, Ord, Read)

data StoreMsg =
  StoreMsgSentVerificationEmail
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- For View.
data CouponView = CouponView
  { couponStore :: Entity Store
  , couponCoupon :: Entity Coupon
  , couponImageUrl :: Maybe Text
  } deriving (Show, Eq)

data CouponViewKey
  = StoreId
  | CouponId

data CouponViewImageUrl
  = CouponImageUrl

data CouponViewTypes
  = Title
  | ValidFrom
  | ValidUntil
  | DiscountPercent
  | DiscountMinimumPrice
  | GiftContent
  | GiftMinimumPrice
  | GiftReferencePrice
  | SetContent
  | SetPrice
  | SetReferencePrice
  | OtherContent

data CouponViewConditions
  = DiscountOtherConditions
  | GiftOtherConditions
  | SetOtherConditions
  | OtherConditions

data CouponViewCouponType =
  CouponType

data StoreView = StoreView
  { storeEntity :: Entity Store
  , storeImageUrl :: Maybe Text
  } deriving (Show, Eq)

data StoreViewText
  = StoreName
  | StoreSalesPoint
  | StoreAddress
  | StorePhoneNumber
  | StoreRegularHoliday
  | StoreUrl

data StoreViewImageUrl
  = StoreImageUrl

data StoreViewBusinessCategory
  = StoreBusinessCategory

data StoreViewBusinessCategoryDetails
  = StoreBusinessCategoryDetails

data StoreViewTexts
  = StoreBusinessHour


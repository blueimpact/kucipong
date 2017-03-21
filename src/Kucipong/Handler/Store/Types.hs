module Kucipong.Handler.Store.Types
  ( StoreError(..)
  , StoreMsg(..)
  , CouponView(..)
  , CouponViewKey(..)
  , CouponViewTypes(..)
  , CouponViewConditions(..)
  , CouponViewCouponType(..)
  ) where

import Kucipong.Prelude

import Database.Persist (Entity(..))

import Kucipong.Db (Coupon(..), Store(..))

-- For I18n.
data StoreError
  = StoreErrorBusinessCategoryDetailIncorrect
  | StoreErrorCouldNotSendEmail
  | StoreErrorCouldNotUploadImage
  | StoreErrorNoImage
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

data CouponViewTypes
  = StoreName
  | StoreAddress
  | ImageUrl
  | Title
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

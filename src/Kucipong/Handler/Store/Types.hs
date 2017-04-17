{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store.Types
  ( StoreError(..)
  , StoreMsg(..)
  , CouponViewText(..)
  , CouponViewTexts(..)
  , CouponViewCouponType(..)
  , StoreViewBusinessCategory(..)
  , StoreViewBusinessCategoryDetails(..)
  , StoreViewText(..)
  , StoreViewTexts(..)
  ) where

import Kucipong.Prelude

import Data.Aeson.TH (defaultOptions, deriveJSON)

-- For I18n.
data StoreError
  = StoreErrorBusinessCategoryDetailIncorrect
  | StoreErrorCouldNotSendEmail
  | StoreErrorCouldNotUploadImage
  | StoreErrorCouponNotFound
  | StoreErrorImageOwnedByStore
  | StoreErrorNoStore
  | StoreErrorNoStoreEmail EmailAddress
  | StoreErrorNotAnImage
  deriving (Show, Eq, Ord, Read)

$(deriveJSON defaultOptions ''StoreError)

data StoreMsg =
  StoreMsgSentVerificationEmail
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

data CouponViewText
  = CouponTitle
  | CouponValidFrom
  | CouponValidUntil
  | CouponDiscountPercent
  | CouponDiscountMinimumPrice
  | CouponGiftContent
  | CouponGiftMinimumPrice
  | CouponGiftReferencePrice
  | CouponSetContent
  | CouponSetPrice
  | CouponSetReferencePrice
  | CouponOtherContent

data CouponViewTexts
  = CouponDiscountOtherConditions
  | CouponGiftOtherConditions
  | CouponSetOtherConditions
  | CouponOtherConditions

data CouponViewCouponType =
  CouponCouponType

data StoreViewText
  = StoreName
  | StoreSalesPoint
  | StoreAddress
  | StorePhoneNumber
  | StoreRegularHoliday
  | StoreUrl

data StoreViewBusinessCategory
  = StoreBusinessCategory

data StoreViewBusinessCategoryDetails
  = StoreBusinessCategoryDetails

data StoreViewTexts
  = StoreBusinessHour

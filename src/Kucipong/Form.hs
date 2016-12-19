{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Kucipong.Form
Description : Data types representing data that can be sent to POST handlers.
Stability   : experimental
Portability : POSIX
-}

module Kucipong.Form where

import Kucipong.Prelude

import Control.Lens ((.~))
import Control.Lens.TH (makeLensesFor)
import Web.FormUrlEncoded (FromForm)

import Kucipong.Db
       (BusinessCategory(..), BusinessCategoryDetail(..), CouponType(..),
        Percent(..), Price(..))

-----------
-- Admin --
-----------

data AdminLoginForm = AdminLoginForm
  { email :: !EmailAddress
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

instance FromForm AdminLoginForm

data AdminStoreCreateForm = AdminStoreCreateForm
  { storeEmail :: !EmailAddress
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

instance FromForm AdminStoreCreateForm

data AdminStoreDeleteConfirmForm = AdminStoreDeleteConfirmForm
  { storeEmail :: !EmailAddress
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

instance FromForm AdminStoreDeleteConfirmForm

data AdminStoreDeleteForm = AdminStoreDeleteForm
  { storeEmail :: !EmailAddress
  , storeName :: !Text
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

instance FromForm AdminStoreDeleteForm

-----------
-- Store --
-----------

data StoreLoginForm = StoreLoginForm
  { email :: !EmailAddress
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

instance FromForm StoreLoginForm

data StoreEditForm = StoreEditForm
  { name :: !Text
  , businessCategory :: !BusinessCategory
  , businessCategoryDetails :: ![BusinessCategoryDetail]
  , salesPoint :: !(Maybe Text)
  , address :: !(Maybe Text)
  , phoneNumber :: !(Maybe Text)
  , businessHours :: !(Maybe Text)
  , regularHoliday :: !(Maybe Text)
  , url :: !(Maybe Text)
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

instance FromForm StoreEditForm

------------------
-- Store Coupon --
------------------

data StoreNewCouponForm = StoreNewCouponForm
  { title :: !Text
  , couponType :: CouponType
  , validFrom :: !UTCTime
  , validUntil :: !(Maybe UTCTime)
  , discountPercent :: !(Maybe Percent)
  , discountMinimumPrice :: !(Maybe Price)
  , discountOtherConditions :: !(Maybe Text)
  , giftContent :: !(Maybe Text)
  , giftReferencePrice :: !(Maybe Price)
  , giftMinimumPrice :: !(Maybe Price)
  , giftOtherConditions :: !(Maybe Text)
  , setContent :: !(Maybe Text)
  , setPrice :: !(Maybe Price)
  , setReferencePrice :: !(Maybe Price)
  , setOtherConditions :: !(Maybe Text)
  , otherContent :: !(Maybe Text)
  , otherConditions :: !(Maybe Text)
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

$(makeLensesFor
    [ ("discountPercent", "discountPercentLens")
    , ("discountMinimumPrice", "discountMinimumPriceLens")
    , ("discountOtherConditions", "discountOtherConditionsLens")
    , ("giftContent", "giftContentLens")
    , ("giftReferencePrice", "giftReferencePriceLens")
    , ("giftMinimumPrice", "giftMinimumPriceLens")
    , ("giftOtherConditions", "giftOtherConditionsLens")
    , ("setContent", "setContentLens")
    , ("setPrice", "setPriceLens")
    , ("setReferencePrice", "setReferencePriceLens")
    , ("setOtherConditions", "setOtherConditionsLens")
    , ("otherContent", "otherContentLens")
    , ("otherConditions", "otherConditionsLens")
    ]
    ''StoreNewCouponForm)

instance FromForm StoreNewCouponForm

removeDiscountCouponInfo :: StoreNewCouponForm -> StoreNewCouponForm
removeDiscountCouponInfo =
  (discountPercentLens .~ Nothing) .
  (discountMinimumPriceLens .~ Nothing) .
  (discountOtherConditionsLens .~ Nothing)

removeGiftCouponInfo :: StoreNewCouponForm -> StoreNewCouponForm
removeGiftCouponInfo =
  (giftContentLens .~ Nothing) .
  (giftReferencePriceLens .~ Nothing) .
  (giftMinimumPriceLens .~ Nothing) . (giftOtherConditionsLens .~ Nothing)

removeSetCouponInfo :: StoreNewCouponForm -> StoreNewCouponForm
removeSetCouponInfo =
  (setContentLens .~ Nothing) .
  (setPriceLens .~ Nothing) .
  (setReferencePriceLens .~ Nothing) . (setOtherConditionsLens .~ Nothing)

removeOtherCouponInfo :: StoreNewCouponForm -> StoreNewCouponForm
removeOtherCouponInfo =
  (otherContentLens .~ Nothing) . (otherConditionsLens .~ Nothing)

-- | Remove the information for the non-used coupon types.
--
-- For example, if the 'CouponType' for this 'StoreNewCouponForm' is
-- 'CouponTypeDiscount', then set 'getContent', 'getReferencePrice',
-- 'giftMinimumPrice', 'giftOtherConditions', 'setContent', etc to 'Nothing'.
removeNonUsedCouponInfo :: StoreNewCouponForm -> StoreNewCouponForm
removeNonUsedCouponInfo storeNewCouponForm =
  case couponType storeNewCouponForm of
    CouponTypeDiscount ->
      removeGiftCouponInfo . removeSetCouponInfo $
      removeOtherCouponInfo storeNewCouponForm
    CouponTypeGift ->
      removeDiscountCouponInfo . removeSetCouponInfo $
      removeOtherCouponInfo storeNewCouponForm
    CouponTypeSet ->
      removeDiscountCouponInfo . removeGiftCouponInfo $
      removeOtherCouponInfo storeNewCouponForm
    CouponTypeOther ->
      removeDiscountCouponInfo . removeGiftCouponInfo $
      removeSetCouponInfo storeNewCouponForm



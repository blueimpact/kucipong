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
import Control.Lens.TH (makeLensesFor, makeWrapped)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (FromHttpApiData(..))

import Kucipong.Db
       (BusinessCategory(..), BusinessCategoryDetail(..), Image(..),
        Key(..), CouponType(..), Percent(..), Price(..))

newtype MaybeEmpty a = MaybeEmpty
  { unMaybeEmpty :: Maybe a
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

$(makeWrapped ''MaybeEmpty)

instance FromHttpApiData a => FromHttpApiData (MaybeEmpty a) where
  parseQueryParam :: Text -> Either Text (MaybeEmpty a)
  parseQueryParam "" = Right (MaybeEmpty Nothing)
  parseQueryParam x = MaybeEmpty . Just <$> parseQueryParam x

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

data AdminStoreLoginForm = AdminStoreLoginForm
  { storeEmail :: !EmailAddress
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

instance FromForm AdminStoreLoginForm

-----------
-- Store --
-----------

data StoreLoginForm = StoreLoginForm
  { email :: !EmailAddress
  } deriving (Data, Eq, Generic, Read, Show, Typeable)

instance FromForm StoreLoginForm

data StoreEditForm = StoreEditForm
  { name :: !(Maybe Text)
  , businessCategory :: !(Maybe BusinessCategory)
  , businessCategoryDetails :: ![BusinessCategoryDetail]
  , salesPoint :: !(Maybe Text)
  , address :: !(Maybe Text)
  , phoneNumber :: !(Maybe Text)
  , businessHours :: !(Maybe Text)
  , regularHoliday :: !(Maybe Text)
  , url :: !(Maybe Text)
  } deriving (Eq, Generic, Read, Show, Typeable)

instance FromForm StoreEditForm

data StoreSetImageForm = StoreSetImageForm
  { imageKey :: !(Maybe (Key Image))
  } deriving (Eq, Generic, Read, Show, Typeable)

$(deriveJSON defaultOptions ''StoreSetImageForm)

------------------
-- Store Coupon --
------------------

data StoreNewCouponForm = StoreNewCouponForm
  { title :: !Text
  , couponType :: CouponType
  , validFrom :: !(MaybeEmpty Day)
  , validUntil :: !(MaybeEmpty Day)
  , discountPercent :: !(MaybeEmpty Percent)
  , discountMinimumPrice :: !(MaybeEmpty Price)
  , discountOtherConditions :: !(MaybeEmpty Text)
  , giftContent :: !(MaybeEmpty Text)
  , giftReferencePrice :: !(MaybeEmpty Price)
  , giftMinimumPrice :: !(MaybeEmpty Price)
  , giftOtherConditions :: !(MaybeEmpty Text)
  , setContent :: !(MaybeEmpty Text)
  , setPrice :: !(MaybeEmpty Price)
  , setReferencePrice :: !(MaybeEmpty Price)
  , setOtherConditions :: !(MaybeEmpty Text)
  , otherContent :: !(MaybeEmpty Text)
  , otherConditions :: !(MaybeEmpty Text)
  } deriving (Eq, Generic, Read, Show, Typeable)

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
  (discountPercentLens .~ (MaybeEmpty Nothing)) .
  (discountMinimumPriceLens .~ (MaybeEmpty Nothing)) .
  (discountOtherConditionsLens .~ (MaybeEmpty Nothing))

removeGiftCouponInfo :: StoreNewCouponForm -> StoreNewCouponForm
removeGiftCouponInfo =
  (giftContentLens .~ (MaybeEmpty Nothing)) .
  (giftReferencePriceLens .~ (MaybeEmpty Nothing)) .
  (giftMinimumPriceLens .~ (MaybeEmpty Nothing)) .
  (giftOtherConditionsLens .~ (MaybeEmpty Nothing))

removeSetCouponInfo :: StoreNewCouponForm -> StoreNewCouponForm
removeSetCouponInfo =
  (setContentLens .~ (MaybeEmpty Nothing)) .
  (setPriceLens .~ (MaybeEmpty Nothing)) .
  (setReferencePriceLens .~ (MaybeEmpty Nothing)) .
  (setOtherConditionsLens .~ (MaybeEmpty Nothing))

removeOtherCouponInfo :: StoreNewCouponForm -> StoreNewCouponForm
removeOtherCouponInfo =
  (otherContentLens .~ (MaybeEmpty Nothing)) .
  (otherConditionsLens .~ (MaybeEmpty Nothing))

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



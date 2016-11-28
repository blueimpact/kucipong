{-|
Module      : Kucipong.Form
Description : Data types representing data that can be sent to POST handlers.
Stability   : experimental
Portability : POSIX
-}

module Kucipong.Form where

import Kucipong.Prelude

import Web.FormUrlEncoded (FromForm)

-----------
-- Admin --
-----------

data AdminLoginForm = AdminLoginForm
  { email :: !EmailAddress
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm AdminLoginForm

data AdminStoreCreateForm = AdminStoreCreateForm
  { storeEmail :: !EmailAddress
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm AdminStoreCreateForm

data AdminStoreDeleteConfirmForm = AdminStoreDeleteConfirmForm
  { storeEmail :: !EmailAddress
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm AdminStoreDeleteConfirmForm

data AdminStoreDeleteForm = AdminStoreDeleteForm
  { storeEmail :: !EmailAddress
  , storeName :: !Text
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm AdminStoreDeleteForm

-----------
-- Store --
-----------

data StoreLoginForm = StoreLoginForm
  { email :: !EmailAddress
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm StoreLoginForm

data StoreEditForm = StoreEditForm
  { name :: !Text
  , businessCategory :: !Text
  , businessCategoryDetails :: ![Text]
  , salesPoint :: !(Maybe Text)
  , address :: !(Maybe Text)
  , phoneNumber :: !(Maybe Text)
  , businessHours :: !(Maybe Text)
  , regularHoliday :: !(Maybe Text)
  , url :: !(Maybe Text)
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm StoreEditForm

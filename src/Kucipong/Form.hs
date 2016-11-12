{-|
Module      : Kucipong.Form
Description : Data types representing data that can be sent to POST handlers.
Stability   : experimental
Portability : POSIX
-}

module Kucipong.Form where

import Kucipong.Prelude

import Web.FormUrlEncoded (FromForm)

data AdminLoginForm = AdminLoginForm
  { email :: EmailAddress
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm AdminLoginForm

data AdminStoreCreateForm = AdminStoreCreateForm
  { storeEmail :: EmailAddress
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm AdminStoreCreateForm

data AdminStoreDeleteConfirmForm = AdminStoreDeleteConfirmForm
  { storeEmail :: EmailAddress
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm AdminStoreDeleteConfirmForm

data AdminStoreDeleteForm = AdminStoreDeleteForm
  { storeEmail :: EmailAddress
  , storeName :: Text
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm AdminStoreDeleteForm

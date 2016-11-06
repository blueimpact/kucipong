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
  { emailFormParam :: EmailAddress
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm AdminLoginForm

data AdminStoreCreateForm = AdminStoreCreateForm
  { storeEmailFormParam :: EmailAddress
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm AdminStoreCreateForm

data AdminStoreDeleteConfirmForm = AdminStoreDeleteConfirmForm
  { storeEmailFormParam :: EmailAddress
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm AdminStoreDeleteConfirmForm

data AdminStoreDeleteForm = AdminStoreDeleteForm
  { storeEmailFormParam :: EmailAddress
  , storeNameFormParam :: Text
  } deriving (Data, Eq, Generic, Show, Typeable)

instance FromForm AdminStoreDeleteForm

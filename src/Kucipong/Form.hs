{-|
Module      : Kucipong.Form
Description : Data types representing data that can be sent to POST handlers.
Stability   : experimental
Portability : POSIX
-}

module Kucipong.Form where

import Kucipong.Prelude


data AdminStoreCreateForm = AdminStoreCreateForm { adminStoreCreateFormEmail :: EmailAddress }

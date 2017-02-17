module Kucipong.Handler.Store.Types
  ( StoreError(..)
  , StoreMsg(..)
  ) where

import Kucipong.Prelude

data StoreError
  = StoreErrorBusinessCategoryDetailIncorrect
  | StoreErrorCouldNotSendEmail
  | StoreErrorCouldNotUploadImage
  | StoreErrorNoImage
  | StoreErrorNoStoreEmail EmailAddress
  | StoreErrorNotAnImage
  deriving (Show, Eq, Ord, Read)

data StoreMsg
  = StoreMsgSentVerificationEmail
  deriving (Show, Eq, Ord, Read, Enum, Bounded)


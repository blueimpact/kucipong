module Kucipong.Handler.Store.Types
  ( StoreError(..)
  , StoreMsg(..)
  ) where

import Kucipong.Prelude

data StoreError
  = StoreErrorCouldNotSendEmail
  | StoreErrorNoImage
  | StoreErrorNoStoreEmail EmailAddress
  deriving (Show, Eq, Ord, Read)

data StoreMsg
  = StoreMsgSentVerificationEmail
  deriving (Show, Eq, Ord, Read, Enum, Bounded)


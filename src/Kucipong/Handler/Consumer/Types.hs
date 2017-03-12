module Kucipong.Handler.Consumer.Types
  ( ConsumerError(..)
  -- , ConsumerMsg(..)
  ) where

import Kucipong.Prelude

data ConsumerError
  = ConsumerErrorCouldNotFindCoupon
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- data ConsumerMsg
--   = ConsumerMsg
--   deriving (Show, Eq, Ord, Read, Enum, Bounded)

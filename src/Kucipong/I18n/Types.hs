module Kucipong.I18n.Types
  ( Lang(..)
  ) where

import Kucipong.Prelude

import Data.Default (Default(..))

-- | Language to show web page contents in
data Lang
  = EnUS
  deriving (Show, Eq, Read, Ord, Enum, Bounded)

-- | Default language for rendering pages.
-- In development phase, we use 'EnUS' as default temporary.
instance Default Lang where
  def = EnUS

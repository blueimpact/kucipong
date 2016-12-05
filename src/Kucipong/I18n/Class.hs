module Kucipong.I18n.Class where

import Kucipong.Prelude

import Kucipong.I18n.Types (Lang)

-- | The label name of a data to show in web page.
class I18n a where
  label :: Lang -> a -> Text

module Kucipong.View.Class where

import Kucipong.Prelude

type family ViewO a :: *

-- | Way to format data on front-end side.
class View o t where
  format :: t -> o -> ViewO t

-- | Way to format data type as a @name@ attribute.
class ToName t where
  toName :: t -> Text

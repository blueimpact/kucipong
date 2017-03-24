module Kucipong.View.Class where

-- | Way to format data on front-end side.
class View o t where
  type ViewO t
  format :: t -> o -> ViewO t

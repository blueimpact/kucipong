module Kucipong.View.Class where

type family ViewO a :: *

-- | Way to format data on front-end side.
class View o t where
  format :: t -> o -> ViewO t

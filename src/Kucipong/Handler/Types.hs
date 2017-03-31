module Kucipong.Handler.Types
  ( PageViewer(..)
  ) where

import Kucipong.Prelude

-- | The user who is viewing a given webpage.
data PageViewer
  = PageViewerAdmin
  | PageViewerEndUser
  | PageViewerStore
  deriving (Bounded, Data, Enum, Eq, Read, Show, Typeable)



module Kucipong.Util where

import Kucipong.Prelude

(<$$>) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
(<$$>) = fmap . fmap

infixr 8 <$$>


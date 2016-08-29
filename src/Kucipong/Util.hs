
module Kucipong.Util where

import Kucipong.Prelude

import Data.Time.Clock ( NominalDiffTime, addUTCTime )

-- | Double 'fmap'.
--
-- >>> succ <$$> Just (Just 3)
-- Just (Just 4)
(<$$>) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
(<$$>) = fmap . fmap

infixr 8 <$$>

oneDay :: NominalDiffTime
oneDay = 60 * 60 * 24

-- | Add one day to a 'UTCTime'.
addOneDay :: UTCTime -> UTCTime
addOneDay = addUTCTime oneDay


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

-- | Similar to 'fromMaybe'.  @'fromMaybeM' action@ is the same as @'maybe'
-- action 'pure'@.
--
-- >>> fromMaybeM (putStrLn "hello") Nothing
-- hello
-- >>> fromMaybeM (Left "there was an error") (Just 3)
-- Right 3
fromMaybeM :: Applicative m => m a -> Maybe a -> m a
fromMaybeM action = maybe action pure

-- | Similar to 'fromMaybeM'.  @'fromEitherM' errHandler@ is the same as
-- @'either' errHandler 'pure'@.
--
-- >>> fromEitherM (putStrLn) $ Left "hello"
-- hello
-- >>> fromEitherM (const Nothing) (Right 3)
-- Just 3
fromEitherM :: Applicative m => (e -> m a) -> Either e a -> m a
fromEitherM errHandler = either errHandler pure

oneDay :: NominalDiffTime
oneDay = 60 * 60 * 24

oneYear :: NominalDiffTime
oneYear = oneDay * 365

-- | Add one day to a 'UTCTime'.
addOneDay :: UTCTime -> UTCTime
addOneDay = addUTCTime oneDay

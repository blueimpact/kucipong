{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Description : Orphan instances

This is a hack to make orphan instances of some of the types we use.  This file
will mostly be making persistent ('PersistField' and 'PersistFieldSql') and
aeson ('FromJSON' and 'ToJSON') instances for types we use that are defined in
other libraries but that we use here.
-}

module Kucipong.Orphans where

import ClassyPrelude

import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Random (MonadRandom(..))
import Control.Monad.Trans.Resource (ResourceT)
import Numeric.Natural (Natural)
import Language.Haskell.TH (Q, runIO)
import Text.Blaze (Markup, ToMarkup(..), string)
import Web.Spock (ActionCtxT)

instance MonadRandom m =>
         MonadRandom (LoggingT m) where
  getRandom = lift getRandom
  getRandomR = lift . getRandomR
  getRandoms = lift getRandoms
  getRandomRs = lift . getRandomRs

instance MonadLogger m =>
         MonadLogger (ActionCtxT ctx m)

instance MonadRandom m => MonadRandom (ResourceT m) where
  getRandom = lift getRandom
  getRandomR = lift . getRandomR
  getRandoms = lift getRandoms
  getRandomRs = lift . getRandomRs

instance MonadIO Q where
  liftIO :: IO a -> Q a
  liftIO = runIO

instance ToMarkup Natural where
  toMarkup :: Natural -> Markup
  toMarkup = string . show

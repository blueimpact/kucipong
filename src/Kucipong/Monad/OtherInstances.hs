{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Monad.OtherInstances where

import Kucipong.Prelude

import Control.Monad.Random ( MonadRandom(..) )
import Web.Spock ( ActionCtxT )

instance MonadRandom m => MonadRandom (LoggingT m) where
    getRandom = lift getRandom
    getRandomR = lift . getRandomR
    getRandoms = lift getRandoms
    getRandomRs = lift . getRandomRs

instance MonadLogger m => MonadLogger (ActionCtxT ctx m)


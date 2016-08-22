{-# LANGUAGE UndecidableInstances #-}

module Kucipong.Monad.SendEmail ( module X ) where

import Kucipong.Monad.SendEmail.Class as X
import Kucipong.Monad.SendEmail.Instance as X ()
import Kucipong.Monad.SendEmail.Trans as X

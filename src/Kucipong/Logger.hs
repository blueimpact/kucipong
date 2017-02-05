
module Kucipong.Logger where

import Kucipong.Prelude

import Control.Monad.Logger (LoggingT, runStdoutLoggingT)

runLogger :: MonadIO m => LoggingT m a -> m a
runLogger = runStdoutLoggingT

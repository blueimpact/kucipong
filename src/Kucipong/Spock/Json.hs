{-|
Module      : Kucipong.Spock.Json
Description : Helper functions for json.
Stability   : experimental
Portability : POSIX
-}

module Kucipong.Spock.Json where

import Kucipong.Prelude

import Data.Aeson (ToJSON)
import Web.Envelope (Success(Success), toErr)
import Web.Spock.Core (ActionCtxT, json)

jsonError
  :: (MonadIO m, ToJSON a)
  => a -> Text -> ActionCtxT ctx m b
jsonError a = json . toErr a

jsonSuccess
  :: (MonadIO m, ToJSON a)
  => a -> ActionCtxT ctx m b
jsonSuccess = json . Success

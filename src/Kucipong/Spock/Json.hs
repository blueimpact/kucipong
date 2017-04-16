{-|
Module      : Kucipong.Spock.Json
Description : Helper functions for json.
Stability   : experimental
Portability : POSIX
-}

module Kucipong.Spock.Json where

import Kucipong.Prelude

import Data.Aeson (ToJSON)
import Network.HTTP.Types.Status (Status)
import Web.Envelope (Success(Success), toErr)
import Web.Spock.Core (ActionCtxT, json, setStatus)

jsonError
  :: (MonadIO m, ToJSON a)
  => a -> Text -> ActionCtxT ctx m b
jsonError a = json . toErr a

jsonErrorStatus
  :: (MonadIO m, ToJSON a)
  => Status -> a -> Text -> ActionCtxT ctx m b
jsonErrorStatus status a msg = do
  setStatus status
  jsonError a msg

jsonSuccess
  :: (MonadIO m, ToJSON a)
  => a -> ActionCtxT ctx m b
jsonSuccess = json . Success

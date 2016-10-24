{-|
Module      : Kucipong.Spock.ReqParam
Description : Helper functions for request parameters.
Stability   : experimental
Portability : POSIX
-}

module Kucipong.Spock.ReqParam where

import Kucipong.Prelude

import Control.FromSum (fromEitherM)
import Web.FormUrlEncoded (FromForm(fromForm), toForm)
import Web.Spock (ActionCtxT)
import Web.Spock.Core (params)

-- This should really be 'body', not 'params', but it looks like there is
-- some problem with using 'body'. It doesn't appear to return any data.
getReqParam
  :: forall ctx m a.
     (FromForm a, MonadIO m)
  => ActionCtxT ctx m (Either Text a)
getReqParam = fromForm . toForm <$> params

getReqParamErr
  :: forall ctx m a.
     (FromForm a, MonadIO m)
  => (Text -> ActionCtxT ctx m a) -> ActionCtxT ctx m a
getReqParamErr errHandler = fromEitherM errHandler =<< getReqParam

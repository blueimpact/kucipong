{-|
Module      : Kucipong.Spock.ReqParam
Description : Helper functions for request parameters.
Stability   : experimental
Portability : POSIX
-}

module Kucipong.Spock.ReqParam where

import Kucipong.Prelude

import Control.FromSum ( fromEitherM )
import Data.HVect ( HasRep, HVectElim )
import Web.FormUrlEncoded ( FromForm, urlDecodeAsForm )
import Web.Spock ( ActionCtxT, Path, body, post, redirect, renderRoute, root )
import Web.Spock.Core ( SpockCtxT )

getReqParam
    :: forall ctx m a
     . (FromForm a, MonadIO m)
    => ActionCtxT ctx m a
getReqParam = getReqParamErr handleFormDecodeError
  where
    handleFormDecodeError :: Text -> ActionCtxT ctx m a
    handleFormDecodeError _ =
        -- TODO: How should we handle this error?
        redirect $ renderRoute root

getReqParamErr
    :: forall ctx m a
     . (FromForm a, MonadIO m)
     => (Text -> ActionCtxT ctx m a)
     -> ActionCtxT ctx m a
getReqParamErr errHandler = fromEitherM errHandler . urlDecodeAsForm . fromStrict =<< body

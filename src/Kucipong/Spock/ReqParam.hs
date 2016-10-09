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
import Network.Wai ( requestBody )
import Web.FormUrlEncoded ( FromForm(fromForm), toForm, urlDecodeAsForm )
import Web.Spock ( ActionCtxT, Path, body, post, redirect, renderRoute, root )
import Web.Spock.Core ( SpockCtxT, request, params )

getReqParam
    :: forall ctx m a
     . (FromForm a, MonadIO m)
    => ActionCtxT ctx m a
getReqParam = getReqParamErr handleFormDecodeError
  where
    handleFormDecodeError :: Text -> ActionCtxT ctx m a
    handleFormDecodeError err = do
        -- TODO: How should we handle this error?
        putStrLn $ "Error in getReqParam: " <> err
        redirect $ renderRoute root

getReqParamErr
    :: forall ctx m a
     . (FromForm a, MonadIO m)
    => (Text -> ActionCtxT ctx m a)
    -> ActionCtxT ctx m a
getReqParamErr errHandler = do
    -- This should really be 'body', not 'params', but it looks like there is
    -- some problem with using 'body'. It doesn't appear to return any data.
    p <- params
    let f = toForm p
    let eitherItem = fromForm f
    fromEitherM errHandler eitherItem

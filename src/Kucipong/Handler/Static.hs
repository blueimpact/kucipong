{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Static where

import Kucipong.Prelude

import Web.Spock.Core (SpockCtxT, subcomponent)

import Kucipong.Handler.Route (staticR)
import Kucipong.Handler.Static.TH (staticComponent')

staticComponent
    :: forall m
     . ( MonadIO m)
    => SpockCtxT () m ()
staticComponent =
  subcomponent staticR $(staticComponent')

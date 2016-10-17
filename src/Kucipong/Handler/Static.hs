{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Static where

import Kucipong.Prelude
import Kucipong.Handler.Static.TH ( staticComponent' )
import Web.Routing.Combinators ( PathState(Open) )
import Web.Spock ( Path )
import Web.Spock.Core ( SpockCtxT )

-- | Url prefix for all of the following 'Path's.
staticUrlPrefix :: Path '[] 'Open
staticUrlPrefix = "static"

staticComponent
    :: forall m
     . ( MonadIO m)
    => SpockCtxT () m ()
staticComponent = $(staticComponent')

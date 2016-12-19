
module Kucipong.Handler.Store.Route where

import Web.Routing.Combinators (PathState(Open))
import Web.Spock (Path, (<//>), var)

import Kucipong.LoginToken (LoginToken)

-- | Url prefix for all of the following 'Path's.
storeUrlPrefix :: Path '[] 'Open
storeUrlPrefix = "store"

rootR :: Path '[] 'Open
rootR = ""

loginR :: Path '[] 'Open
loginR = "login"

doLoginR :: Path '[LoginToken] 'Open
doLoginR = loginR <//> var

editR :: Path '[] 'Open
editR = "edit"

couponR :: Path '[] 'Open
couponR = "coupon"


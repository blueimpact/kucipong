
module Kucipong.Handler.Route where

import ClassyPrelude

import Web.Routing.Combinators (PathState(Open))
import Web.Spock (Path, (<//>), var)

import Kucipong.LoginToken (LoginToken)

-----------------
-- Path Pieces --
-----------------

createR :: Path '[] 'Open
createR = "create"

couponR :: Path '[] 'Open
couponR = "coupon"

editR :: Path '[] 'Open
editR = "edit"

loginR :: Path '[] 'Open
loginR = "login"

rootR :: Path '[] 'Open
rootR = ""

storeR :: Path '[] 'Open
storeR = "store"

doLoginR :: Path '[LoginToken] 'Open
doLoginR = loginR <//> var

----------------
-- Full paths --
----------------

storeCouponR :: Path '[] 'Open
storeCouponR = storeR <//> couponR

storeCouponVarEditR :: Path '[Int64] 'Open
storeCouponVarEditR = storeR <//> couponR <//> var <//> editR

storeEditR :: Path '[] 'Open
storeEditR = storeR <//> editR

storeLoginR :: Path '[] 'Open
storeLoginR = storeR <//> loginR


module Kucipong.Handler.Route where

import Web.Routing.Combinators (PathState(Open))
import Web.Spock (Path, (<//>), var)

import Kucipong.LoginToken (LoginToken)
import Kucipong.Db (Coupon, Key(..))

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


----------------
-- Full paths --
----------------

storeCouponR :: Path '[] 'Open
storeCouponR = storeR <//> couponR

storeCouponCreateR :: Path '[] 'Open
storeCouponCreateR = storeR <//> couponR <//> createR

storeCouponVarR :: Path '[Key Coupon] 'Open
storeCouponVarR = storeR <//> couponR <//> var

storeCouponVarEditR :: Path '[Key Coupon] 'Open
storeCouponVarEditR = storeR <//> couponR <//> var <//> editR

storeEditR :: Path '[] 'Open
storeEditR = storeR <//> editR

storeLoginR :: Path '[] 'Open
storeLoginR = storeR <//> loginR

storeLoginVarR :: Path '[LoginToken] 'Open
storeLoginVarR = storeR <//> loginR <//> var

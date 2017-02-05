
module Kucipong.Handler.Route where

import Web.Routing.Combinators (PathState(Open))
import Web.Spock (Path, (<//>), var)

import Kucipong.LoginToken (LoginToken)
import Kucipong.Db (Coupon, Key(..))

-----------------
-- Path Pieces --
-----------------

adminR :: Path '[] 'Open
adminR = "admin"

createR :: Path '[] 'Open
createR = "create"

confirmR :: Path '[] 'Open
confirmR = "confirm"

couponR :: Path '[] 'Open
couponR = "coupon"

deleteR :: Path '[] 'Open
deleteR = "delete"

editR :: Path '[] 'Open
editR = "edit"

loginR :: Path '[] 'Open
loginR = "login"

rootR :: Path '[] 'Open
rootR = ""

staticR :: Path '[] 'Open
staticR = "static"

storeR :: Path '[] 'Open
storeR = "store"

----------------
-- Full paths --
----------------

adminLoginR :: Path '[] 'Open
adminLoginR = adminR <//> loginR

adminLoginVarR :: Path '[LoginToken] 'Open
adminLoginVarR = adminR <//> loginR <//> var

adminStoreCreateR :: Path '[] 'Open
adminStoreCreateR = adminR <//> storeR <//> createR

adminStoreDeleteR :: Path '[] 'Open
adminStoreDeleteR = adminR <//> storeR <//> deleteR

adminStoreDeleteConfirmR :: Path '[] 'Open
adminStoreDeleteConfirmR = adminR <//> storeR <//> deleteR <//> confirmR

adminStoreLoginR :: Path '[] 'Open
adminStoreLoginR = adminR <//> storeR <//> loginR

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

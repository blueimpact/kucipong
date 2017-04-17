
module Kucipong.Handler.Route where

import Web.Routing.Combinators (PathState(Open))
import Web.Spock (Path, (<//>), var)

import Kucipong.LoginToken (LoginToken)
import Kucipong.Db (Coupon, Key(..), Store)

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

imageR :: Path '[] 'Open
imageR = "image"

loginR :: Path '[] 'Open
loginR = "login"

rootR :: Path '[] 'Open
rootR = ""

setR :: Path '[] 'Open
setR = "set"

staticR :: Path '[] 'Open
staticR = "static"

storeR :: Path '[] 'Open
storeR = "store"

consumerR :: Path '[] 'Open
consumerR = ""

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

storeCouponDeleteR :: Path '[] 'Open
storeCouponDeleteR = storeR <//> couponR <//> deleteR

storeCouponDeleteVarR :: Path '[Key Coupon] 'Open
storeCouponDeleteVarR = storeR <//> couponR <//> deleteR <//> var

storeCouponVarR :: Path '[Key Coupon] 'Open
storeCouponVarR = storeR <//> couponR <//> var

storeCouponVarEditR :: Path '[Key Coupon] 'Open
storeCouponVarEditR = storeR <//> couponR <//> var <//> editR

storeCouponVarSetImageR :: Path '[Key Coupon] 'Open
storeCouponVarSetImageR = storeR <//> couponR <//> var <//> setR <//> imageR

storeEditR :: Path '[] 'Open
storeEditR = storeR <//> editR

storeImageR :: Path '[] 'Open
storeImageR = storeR <//> imageR

storeLoginR :: Path '[] 'Open
storeLoginR = storeR <//> loginR

storeLoginVarR :: Path '[LoginToken] 'Open
storeLoginVarR = storeR <//> loginR <//> var

storeSetImageR :: Path '[] 'Open
storeSetImageR = storeR <//> setR <//> imageR

consumerCouponVarR :: Path '[Key Coupon] 'Open
consumerCouponVarR = consumerR <//> couponR <//> var

consumerStoreVarR :: Path '[Key Store] 'Open
consumerStoreVarR = consumerR <//> storeR <//> var

consumerStoreVarCouponR :: Path '[Key Store] 'Open
consumerStoreVarCouponR = consumerR <//> storeR <//> var <//> couponR

{-# LANGUAGE QuasiQuotes #-}

module Kucipong.Db.Models.EntityDefs where

import Kucipong.Prelude

import Database.Persist ( EntityDef )
import Database.Persist.TH ( persistLowerCase )

import Kucipong.Db.Models.Base
       (BusinessCategory, CouponType, CreatedTime, DeletedTime, ImageName,
        LoginTokenExpirationTime, Percent, Price, UpdatedTime)
import Kucipong.LoginToken ( LoginToken )

kucipongEntityDefs :: [EntityDef]
kucipongEntityDefs = [persistLowerCase|
    Admin
        email                   EmailAddress
        created                 CreatedTime
        updated                 UpdatedTime
        deleted                 DeletedTime Maybe
        name                    Text

        Primary email

        deriving Eq
        deriving Show
        deriving Typeable

    AdminLoginToken
        email                   AdminId
        created                 CreatedTime
        updated                 UpdatedTime
        deleted                 DeletedTime Maybe
        loginToken              LoginToken
        expirationTime          LoginTokenExpirationTime

        Primary email

        deriving Eq
        deriving Show
        deriving Typeable

    Store json
        email                   EmailAddress
        created                 CreatedTime
        updated                 UpdatedTime
        deleted                 DeletedTime Maybe
        name                    Text Maybe
        businessCategory        BusinessCategory Maybe
        businessCategoryDetails [BusinessCategoryDetail]
        image                   ImageId Maybe
        salesPoint              Text Maybe
        address                 Text Maybe
        phoneNumber             Text Maybe
        businessHours           Text Maybe
        regularHoliday          Text Maybe
        url                     Text Maybe

        deriving Eq
        deriving Show
        deriving Typeable

    StoreLoginToken
        storeId                 StoreId
        created                 CreatedTime
        updated                 UpdatedTime
        deleted                 DeletedTime Maybe
        loginToken              LoginToken
        expirationTime          LoginTokenExpirationTime

        Primary storeId

        deriving Eq
        deriving Show
        deriving Typeable

    Coupon
        storeId                 StoreId
        created                 CreatedTime
        updated                 UpdatedTime
        deleted                 DeletedTime Maybe
        title                   Text
        couponType              CouponType
        validFrom               Day Maybe
        validUntil              Day Maybe
        image                   ImageId Maybe
        discountPercent         Percent Maybe
        discountMinimumPrice    Price Maybe
        discountOtherConditions Text Maybe
        giftContent             Text Maybe
        giftReferencePrice      Price Maybe
        giftMinimumPrice        Price Maybe
        giftOtherConditions     Text Maybe
        setContent              Text Maybe
        setPrice                Price Maybe
        setReferencePrice       Price Maybe
        setOtherConditions      Text Maybe
        otherContent            Text Maybe
        otherConditions         Text Maybe

        deriving Eq
        deriving Show
        deriving Typeable

    Image
        created                 CreatedTime
        updated                 UpdatedTime
        deleted                 DeletedTime Maybe
        storeId                 StoreId
        s3Name                  ImageName

        deriving Eq
        deriving Show
        deriving Typeable

    |]

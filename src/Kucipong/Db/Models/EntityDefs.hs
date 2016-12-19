{-# LANGUAGE QuasiQuotes #-}

module Kucipong.Db.Models.EntityDefs where

import Kucipong.Prelude

import Database.Persist ( EntityDef )
import Database.Persist.TH ( persistLowerCase )

import Kucipong.Db.Models.Base
       (BusinessCategory, CouponType, CreatedTime, DeletedTime, Image,
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
        storeEmail              StoreEmailId
        created                 CreatedTime
        updated                 UpdatedTime
        deleted                 DeletedTime Maybe
        name                    Text
        businessCategory        BusinessCategory
        businessCategoryDetails [BusinessCategoryDetail]
        image                   Image Maybe
        salesPoint              Text Maybe
        address                 Text Maybe
        phoneNumber             Text Maybe
        businessHours           Text Maybe
        regularHoliday          Text Maybe
        url                     Text Maybe

        Primary storeEmail

        deriving Eq
        deriving Show
        deriving Typeable

    StoreEmail
        email                   EmailAddress
        created                 CreatedTime
        updated                 UpdatedTime
        deleted                 DeletedTime Maybe

        Primary email

        deriving Eq
        deriving Show
        deriving Typeable

    StoreLoginToken
        email                   StoreEmailId
        created                 CreatedTime
        updated                 UpdatedTime
        deleted                 DeletedTime Maybe
        loginToken              LoginToken
        expirationTime          LoginTokenExpirationTime

        Primary email

        deriving Eq
        deriving Show
        deriving Typeable

    Coupon
        storeEmail              StoreEmailId
        created                 CreatedTime
        updated                 UpdatedTime
        deleted                 DeletedTime Maybe
        title                   Text
        couponType              CouponType
        validFrom               UTCTime
        validUntil              UTCTime Maybe
        image                   Image Maybe
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
    |]

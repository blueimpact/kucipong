{-# LANGUAGE QuasiQuotes #-}

module Kucipong.Db.Models.EntityDefs where

import Kucipong.Prelude

import Database.Persist ( EntityDef )
import Database.Persist.TH ( persistLowerCase )

import Kucipong.Db.Models.Base
    ( CouponType, CreatedTime, DeletedTime, Image, LoginTokenExpirationTime
    , Percent, Price, UpdatedTime )
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
        businessCategory        Text
        businessCategoryDetails [Text]
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
        image                   Image
        title                   Text
        validUntil              UTCTime
        couponType              CouponType
        discountPercent         Percent Maybe
        discountMinimumPrice    Price Maybe
        discountRequirements    Text Maybe
        presentDescription      Text Maybe
        presentPrice            Price Maybe
        presentMinimumPrice     Price Maybe
        presentRequirements     Text Maybe
        setDescription          Text Maybe
        setPrice                Price Maybe
        setRequirements         Text Maybe
    |]

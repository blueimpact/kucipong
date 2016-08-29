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

    CompanyEmail
        email                   EmailAddress
        created                 CreatedTime
        updated                 UpdatedTime
        deleted                 DeletedTime Maybe

        Primary email

        deriving Eq
        deriving Show
        deriving Typeable

    Company
        created                 CreatedTime
        updated                 UpdatedTime
        deleted                 DeletedTime Maybe
        emailAddr               CompanyEmailId
        name                    Text
        businessCategory        Text
        businessCategoryDetail  Text
        image                   Image
        salesPoint              Text
        address                 Text
        phoneNumber             Text
        businessHours           Text
        regularHoliday          Text
        url                     Text

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

{-# LANGUAGE QuasiQuotes #-}

module Kucipong.Db.Models.EntityDefs where

import Kucipong.Prelude

import Database.Persist ( EntityDef )
import Database.Persist.TH ( persistLowerCase )

import Kucipong.Db.Models.Base
    ( CreatedTime, DeletedTime, PasswordHash, UpdatedTime )

kucipongEntityDefs :: [EntityDef]
kucipongEntityDefs = [persistLowerCase|
    Session
        validUntil     UTCTime
        userId         UserId

        deriving       Show
        deriving       Typeable

    -- Removed json directive from here because we don't want to send the
    -- password hash when turning a user to JSON.
    User
        created        CreatedTime
        updated        UpdatedTime
        deleted        DeletedTime Maybe
        email          EmailAddress
        passwordHash   PasswordHash

        UniqueUserEmail    email

        deriving       Eq
        deriving       Show
        deriving       Typeable
    |]



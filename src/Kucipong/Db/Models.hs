{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Db.Models
    ( module Kucipong.Db.Models
    , module Kucipong.Db.Models.Base
    , EntityDateFields(..)
    , EntityField(..)
    , Key(..)
    , Unique
    ) where

import Kucipong.Prelude

import Database.Persist
    ( EntityField(..), Key(..), Unique )
import Database.Persist.TH (
    share, mkPersist, sqlSettings, mkMigrate, mpsGenerateLenses,
    )

import Kucipong.Db.Models.Base
    ( CouponType, CreatedTime(..), DeletedTime(..), Image
    , LoginTokenExpirationTime(..), Percent, Price, UpdatedTime(..) )
import Kucipong.Db.Models.EntityDefs ( kucipongEntityDefs )
import Kucipong.LoginToken ( LoginToken )

share [ mkPersist sqlSettings { mpsGenerateLenses = False }
      , mkMigrate "migrateAll"
      ]
      kucipongEntityDefs

emailToStoreKey :: EmailAddress -> Key Store
emailToStoreKey = StoreKey . StoreEmailKey

-- | Type class for getting the 'EntityField' from a record responsible for the
-- 'CreatedTime', 'DeletedTime', and 'UpdatedTime'.
--
-- This is used for writing generic methods for selecting and getting records
-- that don't have a 'DeletedTime', and for automatically updating the
-- 'UpdatedTime' when updating records.
--
-- The 'createdEntityField' is generally not used, but is included for
-- completeness.
class EntityDateFields record where
  createdEntityField :: EntityField record CreatedTime
  deletedEntityField :: EntityField record (Maybe DeletedTime)
  updatedEntityField :: EntityField record UpdatedTime

instance EntityDateFields Admin where
  createdEntityField = AdminCreated
  deletedEntityField = AdminDeleted
  updatedEntityField = AdminUpdated

instance EntityDateFields AdminLoginToken where
  createdEntityField = AdminLoginTokenCreated
  deletedEntityField = AdminLoginTokenDeleted
  updatedEntityField = AdminLoginTokenUpdated

instance EntityDateFields Store where
  createdEntityField = StoreCreated
  deletedEntityField = StoreDeleted
  updatedEntityField = StoreUpdated

instance EntityDateFields StoreLoginToken where
  createdEntityField = StoreLoginTokenCreated
  deletedEntityField = StoreLoginTokenDeleted
  updatedEntityField = StoreLoginTokenUpdated

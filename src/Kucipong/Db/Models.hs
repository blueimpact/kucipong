{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kucipong.Db.Models
    ( module Kucipong.Db.Models
    , module Kucipong.Db.Models.Base
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

share [ mkPersist sqlSettings { mpsGenerateLenses = True }
      , mkMigrate "migrateAll"
      ]
      kucipongEntityDefs

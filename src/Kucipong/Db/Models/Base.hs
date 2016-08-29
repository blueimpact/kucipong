module Kucipong.Db.Models.Base where

import Kucipong.Prelude

import Data.Aeson ( FromJSON, ToJSON )
import Database.Persist ( PersistField(..), PersistValue )
import Database.Persist.Sql ( PersistFieldSql(..), SqlType )
import GHC.Natural ( Natural )

------------
-- Coupon --
------------

data CouponType
    = CouponTypeDiscount
    | CouponTypePresent
    | CouponTypeSet
    deriving
        ( Data, Eq, Generic, Show, Typeable )

couponTypeToText :: CouponType -> Text
couponTypeToText CouponTypeDiscount = "discount"
couponTypeToText CouponTypePresent = "present"
couponTypeToText CouponTypeSet = "set"

couponTypeFromText :: Text -> Either Text CouponType
couponTypeFromText "discount" = pure CouponTypeDiscount
couponTypeFromText "present" = pure CouponTypePresent
couponTypeFromText "set" = pure CouponTypeSet
couponTypeFromText text = Left $ "Tried to convert \"" <> text
    <> "\"to coupon type, but failed."

instance PersistField CouponType where
    toPersistValue :: CouponType -> PersistValue
    toPersistValue = toPersistValue . couponTypeToText

    fromPersistValue :: PersistValue -> Either Text CouponType
    fromPersistValue = couponTypeFromText <=< fromPersistValue

instance PersistFieldSql CouponType where
    sqlType :: Proxy CouponType -> SqlType
    sqlType _ = sqlType (Proxy :: Proxy Text)

------------------------------------
-- Created, modified, delete time --
------------------------------------

newtype CreatedTime = CreatedTime { unCreatedTime :: UTCTime }
    deriving
        ( Data, Eq, FromJSON, Generic, Ord, PersistField, PersistFieldSql, Show
        , ToJSON, Typeable )

newtype UpdatedTime = UpdatedTime { unUpdatedTime :: UTCTime }
    deriving
        ( Data, Eq, FromJSON, Generic, Ord, PersistField, PersistFieldSql, Show
        , ToJSON, Typeable )

newtype DeletedTime = DeletedTime { unDeletedTime :: UTCTime }
    deriving
        ( Data, Eq, FromJSON, Generic, Ord, PersistField, PersistFieldSql, Show
        , ToJSON, Typeable )

-----------
-- Image --
-----------

newtype Image = Image { unImage :: Text }
    deriving
        ( Data, Eq, FromJSON, Generic, Ord, PersistField, PersistFieldSql, Show
        , ToJSON, Typeable )

---------------------------------
-- Login Token Expiration Time --
---------------------------------

newtype LoginTokenExpirationTime = LoginTokenExpirationTime
    { unLoginTokenExpirationTime :: UTCTime }
    deriving
        ( Data, Eq, FromJSON, Generic, Ord, PersistField, PersistFieldSql, Show
        , ToJSON, Typeable )

-------------
-- Percent --
-------------

newtype Percent = Percent { unPercent :: Natural }
    deriving
        ( Data, Eq, Generic, PersistField, PersistFieldSql, Show, Typeable )

-----------
-- Price --
-----------

newtype Price = Price { unPrice :: Int64 }
    deriving
        ( Data, Eq, Generic, PersistField, PersistFieldSql, Show, Typeable )

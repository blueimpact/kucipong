{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Db.Models.Base where

import Kucipong.Prelude

import Data.Aeson ( FromJSON, ToJSON, Value, parseJSON, toJSON )
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Aeson.Types (parseMaybe, typeMismatch)
import Database.Persist ( PersistField(..), PersistValue )
import Database.Persist.Sql ( PersistFieldSql(..), SqlType )
import Database.Persist.TH (derivePersistField)
import GHC.Natural ( Natural )
import Web.Internal.HttpApiData (FromHttpApiData, parseUrlPiece)

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

----------------------
-- BusinessCategory --
----------------------

data BusinessCategory
  = Gourmet
  | Fashion
  | Gadget
  | Traveling
  | Beauty
  deriving ( Bounded
           , Data
           , Enum
           , Eq
           , Generic
           , Show
           , Typeable
           , Read
           , Ord
           )
derivePersistField "BusinessCategory"
deriveJSON defaultOptions ''BusinessCategory

instance FromHttpApiData BusinessCategory where
  parseUrlPiece a = case readMay a of
    Nothing -> Left $ "Tried to convert \"" <> a
      <> "\" to business category, but failed."
    Just x -> pure x

----------------------------
-- BusinessCategoryDetail --
----------------------------

data GourmetDetail
  = GourmetPizza
  | GourmetSushi
  deriving ( Bounded
           , Data
           , Enum
           , Eq
           , Generic
           , Show
           , Typeable
           , Read
           , Ord
           )
derivePersistField "GourmetDetail"
deriveJSON defaultOptions ''GourmetDetail

data FashionDetail
  = FashionWomen
  | FashionMen
  deriving ( Bounded
           , Data
           , Enum
           , Eq
           , Generic
           , Show
           , Typeable
           , Read
           , Ord
           )
derivePersistField "FashionDetail"
deriveJSON defaultOptions ''FashionDetail

data GadgetDetail
  = GadgetCamera
  | GadgetPC
  deriving ( Bounded
           , Data
           , Enum
           , Eq
           , Generic
           , Show
           , Typeable
           , Read
           , Ord
           )
derivePersistField "GadgetDetail"
deriveJSON defaultOptions ''GadgetDetail

data TravelingDetail
  = TravelingAsia
  | TravelingEurope
  deriving ( Bounded
           , Data
           , Enum
           , Eq
           , Generic
           , Show
           , Typeable
           , Read
           , Ord
           )
derivePersistField "TravelingDetail"
deriveJSON defaultOptions ''TravelingDetail

data BeautyDetail
  = BeautyHairSalon
  | BeautySpa
  deriving ( Bounded
           , Data
           , Enum
           , Eq
           , Generic
           , Show
           , Typeable
           , Read
           , Ord
           )
derivePersistField "BeautyDetail"
deriveJSON defaultOptions ''BeautyDetail

data CommonDetail
  = CommonPoliteService
  | CommonChainStore
  deriving ( Bounded
           , Data
           , Enum
           , Eq
           , Generic
           , Show
           , Typeable
           , Read
           , Ord
           )
derivePersistField "CommonDetail"
deriveJSON defaultOptions ''CommonDetail

data BusinessCategoryDetail
  = GourmetDetail GourmetDetail
  | FashionDetail FashionDetail
  | GadgetDetail GadgetDetail
  | TravelingDetail TravelingDetail
  | BeautyDetail BeautyDetail
  | CommonDetail CommonDetail
  deriving (Data, Eq, Generic, Typeable)

instance Show BusinessCategoryDetail where
  show :: BusinessCategoryDetail -> String
  show (GourmetDetail a) = show a
  show (FashionDetail a) = show a
  show (GadgetDetail a) = show a
  show (TravelingDetail a) = show a
  show (BeautyDetail a) = show a
  show (CommonDetail a) = show a

businessCategoryDetailFromText :: Text -> Either Text BusinessCategoryDetail
businessCategoryDetailFromText a = case mDetail of
  Nothing -> Left $ "Tried to convert \"" <> a
    <> "\" to business category detail, but failed."
  Just x -> pure x
 where
  mDetail =
    (GourmetDetail <$> readMay a) <|>
    (FashionDetail <$> readMay a) <|>
    (GadgetDetail <$> readMay a) <|>
    (TravelingDetail <$> readMay a) <|>
    (BeautyDetail <$> readMay a) <|>
    (CommonDetail <$> readMay a)

instance PersistField BusinessCategoryDetail where
    toPersistValue :: BusinessCategoryDetail -> PersistValue
    toPersistValue = toPersistValue . tshow

    fromPersistValue :: PersistValue -> Either Text BusinessCategoryDetail
    fromPersistValue = businessCategoryDetailFromText <=< fromPersistValue

instance PersistFieldSql BusinessCategoryDetail where
    sqlType :: Proxy BusinessCategoryDetail -> SqlType
    sqlType _ = sqlType (Proxy :: Proxy Text)

instance ToJSON BusinessCategoryDetail where
  toJSON :: BusinessCategoryDetail -> Value
  toJSON (GourmetDetail a) = toJSON a
  toJSON (FashionDetail a) = toJSON a
  toJSON (GadgetDetail a) = toJSON a
  toJSON (TravelingDetail a) = toJSON a
  toJSON (BeautyDetail a) = toJSON a
  toJSON (CommonDetail a) = toJSON a

instance FromJSON BusinessCategoryDetail where
  parseJSON x =
    case mdetail of
      Nothing ->
        typeMismatch
          ("Tried to convert \"" <> show x <>
           "\" to business category detail, but failed.")
          x
      Just detail -> pure detail
    where
      mdetail =
        (GourmetDetail <$> parseMaybe parseJSON x) <|>
        (FashionDetail <$> parseMaybe parseJSON x) <|>
        (GadgetDetail <$> parseMaybe parseJSON x) <|>
        (TravelingDetail <$> parseMaybe parseJSON x) <|>
        (BeautyDetail <$> parseMaybe parseJSON x) <|>
        (CommonDetail <$> parseMaybe parseJSON x)

instance FromHttpApiData BusinessCategoryDetail where
  parseUrlPiece = businessCategoryDetailFromText

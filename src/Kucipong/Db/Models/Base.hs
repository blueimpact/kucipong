{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Db.Models.Base where

import Kucipong.Prelude

import Control.FromSum (fromMaybeOrM)
import Data.Aeson ( FromJSON, ToJSON, Value, parseJSON, toJSON )
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Aeson.Types (parseMaybe, typeMismatch)
import Data.Data (toConstr)
import Data.Kind (Constraint)
import Database.Persist ( PersistField(..), PersistValue )
import Database.Persist.Sql ( PersistFieldSql(..), SqlType )
import Database.Persist.TH (derivePersistField)
import Numeric.Natural ( Natural )
import Text.Blaze (ToMarkup)
import Text.Read (Read(readPrec), ReadPrec)
import Web.Internal.HttpApiData
       (FromHttpApiData, ToHttpApiData, parseUrlPiece, toUrlPiece)

------------
-- Coupon --
------------

data CouponType
  = CouponTypeDiscount
  | CouponTypeGift
  | CouponTypeSet
  | CouponTypeOther
  deriving (Data, Eq, Generic, Read, Show, Typeable)

couponTypeToText :: CouponType -> Text
couponTypeToText = tshow

couponTypeFromText :: Text -> Either Text CouponType
couponTypeFromText text =
  fromMaybeOrM (readMay text) $
  Left $ "Tried to convert \"" <> text <> "\" to coupon type, but failed."

instance FromHttpApiData CouponType where
  parseUrlPiece :: Text -> Either Text CouponType
  parseUrlPiece = couponTypeFromText

instance ToHttpApiData CouponType where
  toUrlPiece :: CouponType -> Text
  toUrlPiece = couponTypeToText

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

newtype CreatedTime = CreatedTime
  { unCreatedTime :: UTCTime
  } deriving ( Data
             , Eq
             , FromJSON
             , Generic
             , Ord
             , PersistField
             , PersistFieldSql
             , Read
             , Show
             , ToJSON
             , Typeable
             )

newtype UpdatedTime = UpdatedTime
  { unUpdatedTime :: UTCTime
  } deriving ( Data
             , Eq
             , FromJSON
             , Generic
             , Ord
             , PersistField
             , PersistFieldSql
             , Read
             , Show
             , ToJSON
             , Typeable
             )

newtype DeletedTime = DeletedTime
  { unDeletedTime :: UTCTime
  } deriving ( Data
             , Eq
             , FromJSON
             , Generic
             , Ord
             , PersistField
             , PersistFieldSql
             , Read
             , Show
             , ToJSON
             , Typeable
             )

-----------
-- Image --
-----------

newtype Image = Image
  { unImage :: Text
  } deriving ( Data
             , Eq
             , FromJSON
             , Generic
             , Ord
             , PersistField
             , PersistFieldSql
             , Read
             , Show
             , ToJSON
             , Typeable
             )

---------------------------------
-- Login Token Expiration Time --
---------------------------------

newtype LoginTokenExpirationTime = LoginTokenExpirationTime
  { unLoginTokenExpirationTime :: UTCTime
  } deriving ( Data
             , Eq
             , FromJSON
             , Generic
             , Ord
             , PersistField
             , PersistFieldSql
             , Read
             , Show
             , ToJSON
             , Typeable
             )

-------------
-- Percent --
-------------

newtype Percent = Percent
  { unPercent :: Natural
  } deriving ( Data
             , Eq
             , FromHttpApiData
             , Generic
             , Num
             , PersistField
             , PersistFieldSql
             , Read
             , Show
             , ToHttpApiData
             , ToMarkup
             , Typeable
             )

percentToText :: Percent -> Text
percentToText = tshow . unPercent

-----------
-- Price --
-----------

newtype Price = Price
  { unPrice :: Int64
  } deriving ( Data
             , Eq
             , FromHttpApiData
             , Generic
             , Num
             , PersistField
             , PersistFieldSql
             , Read
             , Show
             , ToMarkup
             , ToHttpApiData
             , Typeable
             )

priceToText :: Price -> Text
priceToText = tshow . unPrice

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
  parseUrlPiece = businessCategoryFromText

-- | Read a 'BusinessCategory' from 'Text'.  Return 'Nothing' if the
-- 'BusinessCategory' cannot be read.
--
-- Use 'readMay' internally.
--
-- >>> readBusinessCategory "Gourmet"
-- Just Gourmet
-- >>> readBusinessCategory "foobar"
-- Nothing
readBusinessCategory :: Text -> Maybe BusinessCategory
readBusinessCategory = readMay

-- | Create a 'BusinessCategory' from a 'Text'.  Return a 'Left' with an error
-- message if the 'Text' can't be turned into a 'BusinessCategory'.
--
-- Use 'readBusinessCategory' internally.
--
-- >>> businessCategoryFromText "Gourmet"
-- Right Gourmet
-- >>> Data.Either.isLeft $ businessCategoryFromText "foobar"
-- True
businessCategoryFromText :: Text -> Either Text BusinessCategory
businessCategoryFromText a =
  fromMaybeOrM (readBusinessCategory a) . Left $
  "Tried to convert \"" <> a <> "\" to a business category, but failed."

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

-- | 'Read' instance for 'BusinessCategoryDetail'.  Just use the underlying
-- 'Read' instances for the individual categories.
--
-- >>> let f = readMay :: Text -> Maybe BusinessCategoryDetail
-- >>> f "CommonPoliteService"
-- Just CommonPoliteService
-- >>> f "GourmetPizza"
-- Just GourmetPizza
-- >>> f "foobar"
-- Nothing
instance Read BusinessCategoryDetail where
  readPrec :: ReadPrec BusinessCategoryDetail
  readPrec =
    unfoldAllBusinessCategoryDetailAlt (Proxy :: Proxy Read) (const readPrec) ()

instance Show BusinessCategoryDetail where
  show :: BusinessCategoryDetail -> String
  show = foldAllBusinessCategoryDetail (Proxy :: Proxy Show) show

class ToBusinessCategoryDetail a where
  toBusinessCategoryDetail :: a -> BusinessCategoryDetail

instance ToBusinessCategoryDetail BusinessCategoryDetail where
  toBusinessCategoryDetail :: BusinessCategoryDetail -> BusinessCategoryDetail
  toBusinessCategoryDetail = id

instance ToBusinessCategoryDetail GourmetDetail where
  toBusinessCategoryDetail :: GourmetDetail -> BusinessCategoryDetail
  toBusinessCategoryDetail = GourmetDetail

instance ToBusinessCategoryDetail FashionDetail where
  toBusinessCategoryDetail :: FashionDetail -> BusinessCategoryDetail
  toBusinessCategoryDetail = FashionDetail

instance ToBusinessCategoryDetail GadgetDetail where
  toBusinessCategoryDetail :: GadgetDetail -> BusinessCategoryDetail
  toBusinessCategoryDetail = GadgetDetail

instance ToBusinessCategoryDetail TravelingDetail where
  toBusinessCategoryDetail :: TravelingDetail -> BusinessCategoryDetail
  toBusinessCategoryDetail = TravelingDetail

instance ToBusinessCategoryDetail BeautyDetail where
  toBusinessCategoryDetail :: BeautyDetail -> BusinessCategoryDetail
  toBusinessCategoryDetail = BeautyDetail

instance ToBusinessCategoryDetail CommonDetail where
  toBusinessCategoryDetail :: CommonDetail -> BusinessCategoryDetail
  toBusinessCategoryDetail = CommonDetail

-- | Church-style fold for 'BusinessCategoryDetail'.
--
-- >>> let businessCategoryDetail = GourmetDetail GourmetSushi
-- >>> foldBusinessCategoryDetail show show show show show show businessCategoryDetail
-- "GourmetSushi"
foldBusinessCategoryDetail
  :: (GourmetDetail -> a)
  -> (FashionDetail -> a)
  -> (GadgetDetail -> a)
  -> (TravelingDetail -> a)
  -> (BeautyDetail -> a)
  -> (CommonDetail -> a)
  -> BusinessCategoryDetail
  -> a
foldBusinessCategoryDetail f _ _ _ _ _ (GourmetDetail a) = f a
foldBusinessCategoryDetail _ f _ _ _ _ (FashionDetail a) = f a
foldBusinessCategoryDetail _ _ f _ _ _ (GadgetDetail a) = f a
foldBusinessCategoryDetail _ _ _ f _ _ (TravelingDetail a) = f a
foldBusinessCategoryDetail _ _ _ _ f _ (BeautyDetail a) = f a
foldBusinessCategoryDetail _ _ _ _ _ f (CommonDetail a) = f a

-- | This function allows you to pass a single function to apply to a
-- 'BusinessCategoryDetail', as long as 'GourmetDetail', 'FashionDetail',
-- 'GadgetDetail', 'TravelingDetail', 'BeautyDetail', and 'CommonDetail' all
-- implement the same typeclass.
--
-- This is useful for implementing the 'show' function.
--
-- >>> let showProxy = Proxy :: Proxy Show
-- >>> foldAllBusinessCategoryDetail showProxy show (GourmetDetail GourmetSushi)
-- "GourmetSushi"
foldAllBusinessCategoryDetail
  :: forall (c :: * -> Constraint) a proxy.
     ( c GourmetDetail
     , c FashionDetail
     , c GadgetDetail
     , c TravelingDetail
     , c BeautyDetail
     , c CommonDetail
     )
  => proxy c -> (forall x . c x => x -> a) -> BusinessCategoryDetail -> a
foldAllBusinessCategoryDetail _ f = foldBusinessCategoryDetail f f f f f f

-- | Unfold for 'BusinessCategoryDetail'.
--
-- >>> let combiner = (<|>) :: Maybe z -> Maybe z -> Maybe z
-- >>> let f = readMay :: Read x => Text -> Maybe x
-- >>> unfoldBusinessCategoryDetail combiner f f f f f f "CommonPoliteService"
-- Just CommonPoliteService
-- >>> unfoldBusinessCategoryDetail combiner f f f f f f "foobar"
-- Nothing
unfoldBusinessCategoryDetail
  :: Functor f
  => (f BusinessCategoryDetail -> f BusinessCategoryDetail -> f BusinessCategoryDetail)
  -- ^ Combining function
  -> (a -> f GourmetDetail)
  -> (a -> f FashionDetail)
  -> (a -> f GadgetDetail)
  -> (a -> f TravelingDetail)
  -> (a -> f BeautyDetail)
  -> (a -> f CommonDetail)
  -> a
  -- ^ seed
  -> f BusinessCategoryDetail
unfoldBusinessCategoryDetail combiner gourmetF fashionF gadgetF travelingF beautyF commonF a =
  fmap toBusinessCategoryDetail (gourmetF a) `combiner`
  fmap toBusinessCategoryDetail (fashionF a) `combiner`
  fmap toBusinessCategoryDetail (gadgetF a) `combiner`
  fmap toBusinessCategoryDetail (travelingF a) `combiner`
  fmap toBusinessCategoryDetail (beautyF a) `combiner`
  fmap toBusinessCategoryDetail (commonF a)

-- | Just like 'unfoldBusinessCategoryDetail', but use '(<|>)' as the combining
-- function.
--
-- >>> let f = readMay :: Read x => Text -> Maybe x
-- >>> unfoldBusinessCategoryDetailAlt f f f f f f "BeautySpa"
-- Just BeautySpa
-- >>> unfoldBusinessCategoryDetailAlt f f f f f f "foobar"
-- Nothing
unfoldBusinessCategoryDetailAlt
  :: Alternative f
  => (a -> f GourmetDetail)
  -> (a -> f FashionDetail)
  -> (a -> f GadgetDetail)
  -> (a -> f TravelingDetail)
  -> (a -> f BeautyDetail)
  -> (a -> f CommonDetail)
  -> a
  -> f BusinessCategoryDetail
unfoldBusinessCategoryDetailAlt = unfoldBusinessCategoryDetail (<|>)

-- | Similar to 'foldAllBusinessCategoryDetail'.
--
-- Helpful to use with functions like 'readMay'.
--
-- >>> let combiner = (<|>) :: Maybe z -> Maybe z -> Maybe z
-- >>> let readProxy = Proxy :: Proxy Read
-- >>> let f = readMay :: Read x => Text -> Maybe x
-- >>> unfoldAllBusinessCategoryDetail readProxy combiner f "TravelingAsia"
-- Just TravelingAsia
-- >>> unfoldAllBusinessCategoryDetail readProxy combiner f "foobar"
-- Nothing
unfoldAllBusinessCategoryDetail
  :: forall (c :: * -> Constraint) a f proxy.
     ( c GourmetDetail
     , c FashionDetail
     , c GadgetDetail
     , c TravelingDetail
     , c BeautyDetail
     , c CommonDetail
     , Functor f
     )
  => proxy c
  -> (f BusinessCategoryDetail -> f BusinessCategoryDetail -> f BusinessCategoryDetail)
  -> (forall x. c x => a -> f x)
  -> a
  -> f BusinessCategoryDetail
unfoldAllBusinessCategoryDetail _ combiner f =
  unfoldBusinessCategoryDetail combiner f f f f f f

-- | See 'unfoldAllBusinessCategoryDetail' and
-- 'unfoldBusinessCategoryDetailAll'.
--
-- Helpful to use with functions like 'readMay'.

-- >>> let readProxy = Proxy :: Proxy Read
-- >>> let f = readMay :: Read x => Text -> Maybe x
-- >>> unfoldAllBusinessCategoryDetail readProxy f "GadgetPC"
-- Just GourmetSushi
-- >>> unfoldAllBusinessCategoryDetail readProxy f "foobar"
-- Nothing
unfoldAllBusinessCategoryDetailAlt
  :: forall (c :: * -> Constraint) a f proxy.
     ( c GourmetDetail
     , c FashionDetail
     , c GadgetDetail
     , c TravelingDetail
     , c BeautyDetail
     , c CommonDetail
     , Alternative f
     )
  => proxy c
  -> (forall x. c x => a -> f x)
  -> a
  -> f BusinessCategoryDetail
unfoldAllBusinessCategoryDetailAlt _ f = unfoldBusinessCategoryDetailAlt f f f f f f

businessCategoryDetailFromText :: Text -> Either Text BusinessCategoryDetail
businessCategoryDetailFromText a =
  fromMaybeOrM mDetail . Left $
  "Tried to convert \"" <> a <> "\" to business category detail, but failed."
  where
    mDetail :: Maybe BusinessCategoryDetail
    mDetail = unfoldAllBusinessCategoryDetailAlt (Proxy :: Proxy Read) readMay a

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
  toJSON = foldAllBusinessCategoryDetail (Proxy :: Proxy ToJSON) toJSON

instance FromJSON BusinessCategoryDetail where
  parseJSON x =
    fromMaybeOrM mdetail $
    typeMismatch
      ("Tried to convert \"" <> show x <>
       "\" to business category detail, but failed.")
      x
    where
      mdetail :: Maybe BusinessCategoryDetail
      mdetail =
        unfoldAllBusinessCategoryDetailAlt
          (Proxy :: Proxy FromJSON)
          (parseMaybe parseJSON)
          x

instance FromHttpApiData BusinessCategoryDetail where
  parseUrlPiece = businessCategoryDetailFromText

-- | Returns whether a given 'BusinessCategoryDetail' works in a
-- 'BusinessCategory'. 'CommonDetail's are valid for every 'BusinessCategory'.
--
-- This currently just compares the 'BusinessCategory' constructor to the first
-- characters of the 'BusinessCategoryDetail' constructor and tests if they are
-- the same.
--
-- For instance, the 'Gourmet' constructor matches the 'GourmetDetail'
-- constructor.
--
-- >>> isValidBusinessCategoryDetailFor Gourmet (GourmetDetail GourmetSushi)
-- True
--
-- However, the 'Fashion' constructor doesn't match the 'GourmetDetail'
-- constructor.
--
-- >>> isValidBusinessCategoryDetailFor Fashion (GourmetDetail GourmetSushi)
-- False
--
-- The 'CommonDetail' constructor works for any 'BusinessCategory'.
--
-- >>> isValidBusinessCategoryDetailFor Gadget (CommonDetail CommonPoliteService)
-- True
isValidBusinessCategoryDetailFor :: BusinessCategory -> BusinessCategoryDetail -> Bool
isValidBusinessCategoryDetailFor busiCat busiCatDet =
  let busiCatConstr = toConstr busiCat
      busiCatDetConstr = toConstr busiCatDet
  in show busiCatConstr `isPrefixOf` show busiCatDetConstr ||
     "Common" `isPrefixOf` show busiCatDetConstr

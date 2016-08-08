{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Description : Orphan instances

This is a hack to make orphan instances of some of the types we use.  This file
will mostly be making persistent ('PersistField' and 'PersistFieldSql') and
aeson ('FromJSON' and 'ToJSON') instances for types we use that are defined in
other libraries (such as 'EmailAddress') but that we us here.
-}

module Kucipong.Orphans where

import ClassyPrelude

import Data.Aeson ( ToJSON(..), FromJSON(..), Value(..), withText )
import Data.Aeson.Types ( Parser )
import Data.Proxy ( Proxy )
import Database.Persist.Postgresql
    ( PersistField(..), PersistFieldSql(..), PersistValue(..), SqlType(..))
import Text.Email.Validate ( EmailAddress, emailAddress, toByteString, validate )
import Web.HttpApiData ( FromHttpApiData(..), ToHttpApiData(..) )
import Web.PathPieces ( PathPiece(..) )

------------------
-- EmailAddress --
------------------

-- | Use 'EmailAddress' as a 'PersistField'.
instance PersistField EmailAddress where
    toPersistValue :: EmailAddress -> PersistValue
    toPersistValue = PersistText . decodeUtf8 . toByteString

    fromPersistValue :: PersistValue -> Either Text EmailAddress
    fromPersistValue (PersistText text) =
        first pack . validate $ encodeUtf8 text
    fromPersistValue x = Left $
        "unexpected value (" <> tshow x <>
            ") in fromPersistValue in EmailAddress instance for PersistField"

-- | Use 'EmailAddress' as a 'PersistField'.
instance PersistFieldSql EmailAddress where
    sqlType :: Proxy EmailAddress -> SqlType
    sqlType _ = SqlString

-- | Turn 'EmailAddress' into JSON.
instance FromJSON EmailAddress where
    parseJSON :: Value -> Parser EmailAddress
    parseJSON = withText "EmailAddress" $ \t ->
                    case validate $ encodeUtf8 t of
                        Left err -> fail $ "Failed to parse email address: " <> err
                        Right email -> return email
    {-# INLINE parseJSON #-}

-- | Turn 'EmailAddress' into JSON.
instance ToJSON EmailAddress where
    toJSON :: EmailAddress -> Value
    toJSON = String . decodeUtf8 . toByteString

instance PathPiece EmailAddress where
    fromPathPiece :: Text -> Maybe EmailAddress
    fromPathPiece = emailAddress . encodeUtf8

    toPathPiece :: EmailAddress -> Text
    toPathPiece = decodeUtf8 . toByteString

instance ToHttpApiData EmailAddress where
    toUrlPiece :: EmailAddress -> Text
    toUrlPiece = decodeUtf8 . toByteString

instance FromHttpApiData EmailAddress where
    parseUrlPiece :: Text -> Either Text EmailAddress
    parseUrlPiece = first pack . validate . encodeUtf8

{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Errors where

import Kucipong.Prelude

import Data.Aeson.TH ( defaultOptions, deriveJSON )
import Database.PostgreSQL.Simple ( SqlError(..) )
import Web.Envelope ( Err(..) )

type AppErr = Err AppErrEnum

instance Exception AppErr

-- | A union of different errors that can occur in our application.
data AppErrEnum
    = AuthErr
    -- ^ An error with auth.
    | HailgunError
    -- ^ An error with sending an email to hailgun.
    | OtherException
    -- ^ A wrapper for any other type of exception
    | SessionExpired
    -- ^ User's session has expired
    | SqlErr
    -- ^ Generic SQL error
    | SqlUniquenessViolation
    -- ^ A uniqueness violation as reported by postgresql-simple.
    | UserEmailAlreadyExists
    -- ^ Email address already exists in database when registering user.
    deriving (Eq, Enum, Generic, Show, Typeable)
deriveJSON defaultOptions ''AppErrEnum

-- | Convert an 'SqlError' to an 'AppErr'.
--
-- TODO: Putting the raw sql error message here may be dangerous.
sqlErrorToAppErr :: SqlError -> AppErr
sqlErrorToAppErr sqlError = Err
    { errErr = SqlErr
    , errExtra = Just $ tshow sqlError
    }

-- | Convert a 'SomeException' to an 'AppErr'.
--
-- TODO: Putting the raw error message here may be dangerous.
someExceptionToAppErr :: SomeException -> AppErr
someExceptionToAppErr someException = Err
    { errErr = OtherException
    , errExtra = Just $ tshow someException
    }

-- | Throw an 'AppErr' with 'throwError' using the 'MonadError' class.
throwAppErr :: (MonadError AppErr m) => AppErrEnum -> Maybe Text -> m a
throwAppErr appErrEnum text = throwError $ Err appErrEnum text

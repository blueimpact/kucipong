module Kucipong.Handler.Admin.Types
  ( AdminError(..)
  , AdminMsg(..)
  ) where

import Kucipong.Prelude

data AdminError
  = AdminErrorCouldNotSendEmail
  | AdminErrorNoAdminEmail
  | AdminErrorNoAdminLoginToken
  | AdminErrorNoAdminSession
  | AdminErrorNoStoreEmail
  | AdminErrorStoreWithSameEmailExists
  | AdminErrorStoreCreateDbProblem
  | AdminErrorSendEmailFailure
  | AdminErrorTokenExpired
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

data AdminMsg
  = AdminMsgSentVerificationEmail
  deriving (Show, Eq, Ord, Read, Enum, Bounded)


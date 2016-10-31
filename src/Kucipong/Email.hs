{-# LANGUAGE QuasiQuotes #-}

module Kucipong.Email where

import Kucipong.Prelude

import Mail.Hailgun
    ( HailgunContext, HailgunErrorMessage, HailgunErrorResponse(..)
    , HailgunMessage, HailgunSendResponse, MessageContent(..)
    , MessageRecipients(..), emptyMessageRecipients, hailgunMessage, sendEmail )
import Network.HTTP.Base ( urlEncode )
import "emailaddress" Text.Email.Validate ( toByteString )
import Text.Shakespeare.Text ( st )

import Kucipong.Host
    ( HasHost, HasProtocol, Host, Protocol, mHost, mProtocol )
import Kucipong.LoginToken ( LoginToken(..) )

class HasHailgunContext r where
    getHailgunContext :: r -> HailgunContext

instance HasHailgunContext HailgunContext where
    getHailgunContext = id

-- | Datatype to represent possible errors with sending an email.
data EmailError
  = EmailErrorIncorrectEmailFormat HailgunErrorMessage
  -- ^ Email was in incorrect format.  Since we are creating emails by hand,
  -- this error should never occur.
  | EmailErrorSendError HailgunErrorResponse
  -- ^ Error from Mailgun when trying to send an email.
  deriving (Generic, Show, Typeable)

-- | Generic method for sending an email.  It takes an 'Either'
-- 'HailgunErrorMessage' 'HailgunMessage'.
--
-- If the value is 'Left' 'HailgunErrorMessage', then throw an
-- 'EmailErrorIncorrectEmailFormat'.  Normally it will not be 'Left', because
-- we are creating the 'HailgunMessage' by hand.
--
-- If the value is 'Right' 'HailgunMessage', then call 'sendEmail'' with the
-- message. 'sendEmail'' may throw a 'HailgunErrorResponse'.  Rethrow this as
-- an 'EmailErrorSendError'.
sendEmailGeneric
    :: forall r m
     . ( HasHailgunContext r
       , MonadIO m
       , MonadReader r m
       )
    => Either HailgunErrorMessage HailgunMessage
    -> m (Either EmailError HailgunSendResponse)
sendEmailGeneric createMessageResult = do
  hailgunContext <- reader getHailgunContext
  let eitherHailgunMessage =
        first EmailErrorIncorrectEmailFormat createMessageResult
      sendMsg msg = first EmailErrorSendError <$> sendEmail' hailgunContext msg
  either (pure . Left) sendMsg eitherHailgunMessage
  where
    sendEmail'
      :: HailgunContext
      -> HailgunMessage
      -> m (Either HailgunErrorResponse HailgunSendResponse)
    sendEmail' = (liftIO .) . sendEmail

sendAdminLoginEmail
    :: forall r m
     . ( HasHailgunContext r
       , HasHost r
       , HasProtocol r
       , MonadIO m
       , MonadReader r m
       )
    => EmailAddress
    -> LoginToken
    -> m (Either EmailError HailgunSendResponse)
sendAdminLoginEmail adminEmail loginToken = do
    protocol <- mProtocol
    host <- mHost
    sendEmailGeneric $ adminLoginMsg protocol host adminEmail loginToken

adminLoginMsg
    :: Protocol
    -> Host
    -> EmailAddress
    -> LoginToken
    -> Either HailgunErrorMessage HailgunMessage
adminLoginMsg protocol host adminEmail loginToken = do
    let loginTokenText =
            asText $ pack $ urlEncode $ unpack $ unLoginToken loginToken
        subject = "Kucipong Admin Login"
        content = TextOnly . encodeUtf8 $ textContent loginTokenText
        replyTo = "no-reply@kucipong.com"
        to = toByteString adminEmail
        recipients = emptyMessageRecipients { recipientsTo = [ to ] }
        attachements = []
    hailgunMessage subject content replyTo recipients attachements
  where
    textContent :: Text -> Text
    textContent loginTokenText =
        [st|
This is an email from Kucipong.  You can use the following URL to
login as an admin:

#{protocol}://#{host}/admin/login/#{loginTokenText}
        |]

sendStoreLoginEmail
    :: forall r m
     . ( HasHailgunContext r
       , HasHost r
       , HasProtocol r
       , MonadIO m
       , MonadReader r m
       )
    => EmailAddress
    -> LoginToken
    -> m (Either EmailError HailgunSendResponse)
sendStoreLoginEmail storeEmail loginToken = do
    protocol <- mProtocol
    host <- mHost
    sendEmailGeneric $ storeLoginMsg protocol host storeEmail loginToken

storeLoginMsg
    :: Protocol
    -> Host
    -> EmailAddress
    -> LoginToken
    -> Either HailgunErrorMessage HailgunMessage
storeLoginMsg protocol host storeEmail loginToken = do
    let loginTokenText =
            asText $ pack $ urlEncode $ unpack $ unLoginToken loginToken
        subject = "Kucipong Store Login"
        content = TextOnly . encodeUtf8 $ textContent loginTokenText
        replyTo = "no-reply@kucipong.com"
        to = toByteString storeEmail
        recipients = emptyMessageRecipients { recipientsTo = [ to ] }
        attachements = []
    hailgunMessage subject content replyTo recipients attachements
  where
    textContent :: Text -> Text
    textContent loginTokenText =
        [st|
This is an email from Kucipong.  You can use the following URL to
login as an store:

#{protocol}://#{host}/store/login/#{loginTokenText}
        |]

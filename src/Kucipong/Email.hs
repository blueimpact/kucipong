{-# LANGUAGE QuasiQuotes #-}

module Kucipong.Email where

import Kucipong.Prelude

import Mail.Hailgun
    ( HailgunContext, HailgunErrorMessage, HailgunErrorResponse(..)
    , HailgunMessage, HailgunSendResponse, MessageContent(..)
    , MessageRecipients(..), emptyMessageRecipients, hailgunMessage, sendEmail )
import "emailaddress" Text.Email.Validate ( toByteString )
import Text.Shakespeare.Text ( st )

import Kucipong.Errors ( AppErr, AppErrEnum(..), throwAppErr )
import Kucipong.LoginToken ( LoginToken(..) )

class HasHailgunContext r where
    getHailgunContext :: r -> HailgunContext

instance HasHailgunContext HailgunContext where
    getHailgunContext = id

-- | Generic method for sending an email.  It takes an 'Either'
-- 'HailgunErrorMessage' 'HailgunMessage'.
--
-- If the value is 'Left' 'HailgunErrorMessage', then throw an
-- 'OtherException'.  Normally it will not be 'Left', because we are creating
-- the 'HailgunMessage' by hand.
--
-- If the value is 'Right' 'HailgunMessage', then call 'sendEmail'' with the
-- message. 'sendEmail'' may throw a 'HailgunErrorResponse'.  Rethrow this as
-- an 'AppErr' ('HailgunError').
sendEmailGeneric
    :: forall r m
     . ( HasHailgunContext r
       , MonadError AppErr m
       , MonadIO m
       , MonadReader r m
       )
    => Either HailgunErrorMessage HailgunMessage
    -> m HailgunSendResponse
sendEmailGeneric createMessageResult = do
    hailgunContext <- reader getHailgunContext
    either handleMessageError (trySendEmail hailgunContext) createMessageResult
  where
    handleMessageError :: String -> m a
    handleMessageError = throwAppErr OtherException . Just . pack

    handleSendError :: HailgunErrorResponse -> m a
    handleSendError = throwAppErr HailgunError . Just . pack . herMessage

    trySendEmail :: HailgunContext -> HailgunMessage -> m HailgunSendResponse
    trySendEmail hailgunContext =
        either handleSendError pure <=< sendEmail' hailgunContext

sendAdminLoginEmail
    :: forall r m
     . ( HasHailgunContext r
       , MonadError AppErr m
       , MonadIO m
       , MonadReader r m
       )
    => EmailAddress
    -> LoginToken
    -> m HailgunSendResponse
sendAdminLoginEmail = (sendEmailGeneric .) . adminLoginMsg

adminLoginMsg :: EmailAddress -> LoginToken -> Either HailgunErrorMessage HailgunMessage
adminLoginMsg adminEmail url =
    let subject = "Kucipong Admin Login"
        content = TextOnly $ encodeUtf8 textContent
        replyTo = "no-reply@kucipong.com"
        to = toByteString adminEmail
        recipients = emptyMessageRecipients { recipientsTo = [ to ] }
        attachements = []
    in hailgunMessage subject content replyTo recipients attachements
  where
    textContent :: Text
    textContent =
        [st|
             This is an email from Kucipong.  You can use the following URL to
             login as an admin:

             TODO: Actually add login email here.
        |]


sendRegistrationCompletedEmail
    :: forall r m
     . ( HasHailgunContext r
       , MonadError AppErr m
       , MonadIO m
       , MonadReader r m
       )
    => EmailAddress -> m HailgunSendResponse
sendRegistrationCompletedEmail = sendEmailGeneric . registrationCompleteMessage

registrationCompleteMessage :: EmailAddress -> Either HailgunErrorMessage HailgunMessage
registrationCompleteMessage companyEmail =
    let subject = "test subject"
        content = TextOnly "test content"
        replyTo = "info@kucipong.com"
        to = toByteString companyEmail
        recipients = emptyMessageRecipients { recipientsTo = [ to ] }
        attachements = []
    in hailgunMessage subject content replyTo recipients attachements

sendEmail'
    :: MonadIO m
    => HailgunContext
    -> HailgunMessage
    -> m (Either HailgunErrorResponse HailgunSendResponse)
sendEmail' = (liftIO .) . sendEmail

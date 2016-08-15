
module Kucipong.Email where

import Kucipong.Prelude

import Mail.Hailgun
    ( HailgunContext, HailgunErrorMessage, HailgunErrorResponse(..)
    , HailgunMessage, HailgunSendResponse, MessageContent(..)
    , MessageRecipients(..), emptyMessageRecipients, hailgunMessage, sendEmail )
import Text.Email.Validate ( toByteString )

import Kucipong.Errors ( AppErr, AppErrEnum(..), throwAppErr )

class HasHailgunContext r where
    getHailgunContext :: r -> HailgunContext

instance HasHailgunContext HailgunContext where
    getHailgunContext = id

sendRegistrationCompletedEmail
    :: forall r m
     . ( HasHailgunContext r
       , MonadError AppErr m
       , MonadIO m
       , MonadReader r m
       )
    => EmailAddress -> m HailgunSendResponse
sendRegistrationCompletedEmail companyEmail = do
    hailgunContext <- reader getHailgunContext
    either handleMessageError (trySendEmail hailgunContext) $
        registrationCompleteMessage companyEmail
  where
    handleMessageError :: String -> m a
    handleMessageError = throwAppErr OtherException . Just . pack

    handleSendError :: HailgunErrorResponse -> m a
    handleSendError = throwAppErr HailgunError . Just . pack . herMessage

    trySendEmail :: HailgunContext -> HailgunMessage -> m HailgunSendResponse
    trySendEmail hailgunContext =
        either handleSendError pure <=< sendEmail' hailgunContext

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

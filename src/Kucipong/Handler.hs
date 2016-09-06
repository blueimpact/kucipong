
module Kucipong.Handler where

import Kucipong.Prelude

import Web.Spock ( Path, (<//>), get, html, root, runSpock, spockT, subcomponent, text, var )

import Kucipong.Config ( Config )
import Kucipong.Handler.Admin ( adminComponent )
import Kucipong.Host ( HasPort(..) )
import Kucipong.Monad ( KucipongM, runKucipongM )

-- TODO: Remove this:
import Kucipong.Email
import Mail.Hailgun
import "emailaddress" Text.Email.Validate (emailAddress)


helloR :: Path '[Text]
helloR = "hello" <//> var

addR :: Path '[Int, Int]
addR = "calculator" <//> var <//> "+" <//> var

runKucipongMHandleErrors :: Config -> KucipongM a -> IO a
runKucipongMHandleErrors config = either throwIO pure <=< runKucipongM config

app :: Config -> IO ()
app config = runSpock (getPort config) $
    spockT (runKucipongMHandleErrors config) $ do
        subcomponent "admin" adminComponent
        get root $ do
            -- dbLoginUser undefined undefined
            html "<p>hello world</p>"
        get helloR $ \name -> text $ "Hello " <> name <> "!"
        get addR $ \a b -> text . pack $ show (a + b)
        -- TODO: Remove this.  This is just an example of how to send email.
        get "sendemail" $ do
            res <- liftIO $ runKucipongM config $ do
                mailgunContext <- reader getHailgunContext
                print mailgunContext
                resp <- sendRegistrationCompletedEmail (fromMaybe undefined $ emailAddress "kucipong.dev@gmail.com")
                print resp
            print res


module Kucipong.Handler where

import Kucipong.Prelude

import Web.Spock ( Path, (<//>), get, html, root, runSpock, spockT, text, var )

import Kucipong.Config ( Config, HasPort(..) )
import Kucipong.Monad ( KucipongM, runKucipongM )

helloR :: Path '[Text]
helloR = "hello" <//> var

addR :: Path '[Int, Int]
addR = "calculator" <//> var <//> "+" <//> var

runKucipongMHandleErrors :: Config -> KucipongM a -> IO a
runKucipongMHandleErrors config = either throwIO pure <=< runKucipongM config

app :: Config -> IO ()
app config = runSpock (getPort config) $
    spockT (runKucipongMHandleErrors config) $ do
        get root $ do
            -- dbLoginUser undefined undefined
            html "<p>hello world</p>"
        get helloR $ \name -> text $ "Hello " <> name <> "!"
        get addR $ \a b -> text $ pack $ show (a + b)

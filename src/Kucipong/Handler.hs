
module Kucipong.Handler where

import Kucipong.Prelude

import Web.Spock ( Path, (<//>), get, html, root, runSpock, spockT, text, var )

import Kucipong.Config ( Config, HasPort(..) )

helloR :: Path '[Text]
helloR = "hello" <//> var

addR :: Path '[Int, Int]
addR = "calculator" <//> var <//> "+" <//> var

app :: Config -> IO ()
app config = runSpock (getPort config) $ spockT id $ do
    get root $ html "<p>hello world</p>"
    get helloR $ \name -> text $ "Hello " <> name <> "!"
    get addR $ \a b -> text $ pack $ show (a + b)

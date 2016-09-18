
module Kucipong.RenderTemplate where

import Kucipong.Prelude

import Language.Haskell.TH ( Exp, Q, litE, stringL )

renderTemplateFromEnv :: String -> Q Exp
renderTemplateFromEnv filename = do
    litE $ stringL "hello"

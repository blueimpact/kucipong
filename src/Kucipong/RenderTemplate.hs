
module Kucipong.RenderTemplate where

import Kucipong.Prelude hiding ( try )

import Control.Exception ( try )
import Language.Haskell.TH ( Exp, Q, litE, stringL )
import Text.EDE ( eitherParse, eitherRender, fromPairs )

import Kucipong.Util ( fromEitherM )

templateDirectory :: FilePath
templateDirectory = "frontend" </> "dist"

renderTemplateFromEnv :: String -> Q Exp
renderTemplateFromEnv filename = do
    eitherRawTemplate <- liftIO . try $ readFile fullFilePath
    rawTemplate <- fromEitherM handleTemplateFileRead eitherRawTemplate
    let eitherParsedTemplate = eitherParse rawTemplate
    -- Check to make sure the template can be parsed correctly.  Return an
    -- error to the user if it cannot.
    void $ fromEitherM handleIncorrectTemplate eitherParsedTemplate
    litE $ stringL "hello"
  where
    -- This is the full path of the template file.
    fullFilePath :: FilePath
    fullFilePath = templateDirectory </> filename

    handleTemplateFileRead :: SomeException -> Q a
    handleTemplateFileRead exception = fail $
        "exception occured when trying to read file \"" <>
        fullFilePath <>
        ": " <>
        show exception

    handleIncorrectTemplate :: String -> Q a
    handleIncorrectTemplate errorMsg = undefined

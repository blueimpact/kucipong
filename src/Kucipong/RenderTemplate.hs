{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Kucipong.RenderTemplate where

import           Kucipong.Prelude           hiding (try)

import           Control.Exception          (try)
import           Control.FromSum            (fromEitherM)
import qualified Data.HashMap.Strict        as HashMap
import           Language.Haskell.TH        (Exp, Q, appE, litE,
                                             lookupValueName, mkName, stringL,
                                             varE)
import           Language.Haskell.TH.Syntax (addDependentFile)
import           Network.HTTP.Types         (internalServerError500)
import           Text.EDE                   (Template, eitherParse,
                                             eitherRenderWith, fromPairs)
import           Text.EDE.Filters           (Term, (@:))
import           Web.Spock                  (html, setStatus)

templateDirectory :: FilePath
templateDirectory = "frontend" </> "dist"

unsafeFromRight :: Show e => Either e a -> a
unsafeFromRight (Right a) = a
unsafeFromRight (Left err) = error $
    "Called unsafeFromRight, but we got a left with this value: " <> show err

renderTemplateFromEnv :: String -> Q Exp
renderTemplateFromEnv filename = do
    addDependentFile fullFilePath
    eitherRawTemplate <- liftIO . try $ readFile fullFilePath
    rawTemplate <- fromEitherM handleTemplateFileRead eitherRawTemplate
    let eitherParsedTemplate = eitherParse rawTemplate
    -- Check to make sure the template can be parsed correctly.  Return an
    -- error during compile time to the user if it cannot.
    void $ fromEitherM handleIncorrectTemplate eitherParsedTemplate
    templateExp <- [e| unsafeFromRight $ eitherParse rawTemplate |]
    eitherRenderLambdaExp <- [e|
      let
        escapeHtml :: Text -> Text
        escapeHtml = concatMap $ \c -> case c of
            '"' -> "&quot;"
            '\'' -> "&#39;"
            '&' -> "&amp;"
            '<' -> "&lt;"
            '>' -> "&gt;"
            _ -> singleton c
        filters :: HashMap Text Term
        filters = HashMap.fromList [ "escape" @: escapeHtml ]
      in
        eitherRenderWith filters $(pure templateExp) |]
    [e| \environmentObject -> case $(pure eitherRenderLambdaExp) environmentObject of
            Left err -> do
                setStatus internalServerError500
                html $ "<h3>Error with rendering template:</h3>" <> pack err
            Right bytestring -> html $ toStrict bytestring |]
  where
    -- This is the full path of the template file.
    fullFilePath :: FilePath
    fullFilePath = templateDirectory </> filename

    handleTemplateFileRead :: SomeException -> Q a
    handleTemplateFileRead exception = fail $
        "exception occured when trying to read the template file \"" <>
        fullFilePath <>
        ":\n" <>
        show exception

    handleIncorrectTemplate :: String -> Q a
    handleIncorrectTemplate errorMsg = fail $
        "exception occured when trying to parse the template file \"" <>
        fullFilePath <> ":\n" <> errorMsg

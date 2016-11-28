{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Kucipong.RenderTemplate where

import           Kucipong.Prelude           hiding (try)

import           Text.Blaze.Renderer.Text   (renderMarkup)
import           Control.Exception          (try)
import           Control.FromSum            (fromEitherM)
import           Data.Aeson                 (Object, (.=))
import qualified Data.HashMap.Strict        as HashMap
import           Language.Haskell.TH        (Exp, Q, appE)
import           Language.Haskell.TH.Syntax (addDependentFile)
import           Network.HTTP.Types         (internalServerError500)
import           Text.EDE                   (eitherParse, eitherRenderWith, fromPairs)
import           Text.EDE.Filters           (Term, (@:))
import           Text.Heterocephalus        (compileHtmlFileWithDefault)
import           Web.Spock                  (html, setStatus)

-- | Add empty @errors@ and @messages@ keys/values to an object if they don't
-- already exist.
addErrorsAndMessages :: Object -> Object
addErrorsAndMessages obj =
    union obj $
    fromPairs ["errors" .= (empty :: [Text]), "messages" .= (empty :: [Text])]

templateDirectory :: FilePath
templateDirectory = "frontend" </> "dist"

unsafeFromRight :: Show e => Either e a -> a
unsafeFromRight (Right a) = a
unsafeFromRight (Left err) = error $
    "Called unsafeFromRight, but we got a left with this value: " <> show err

-- | Render a template file with adding empty @errors@ and @messages@ keys/values
-- if they don't already exist in scope.
renderTemplateFromEnv :: String -> Q Exp
renderTemplateFromEnv filename = renderer `appE` body
  where
    -- This is the full path of the template file.
    fullFilePath :: FilePath
    fullFilePath = templateDirectory </> filename

    body :: Q Exp
    body =
      compileHtmlFileWithDefault
        fullFilePath
        [("errors", [|empty :: [Text]|]), ("messages", [|empty :: [Text]|])]

    renderer :: Q Exp
    renderer = [|html . toStrict . renderMarkup|]

{-# DEPRECATED renderTemplateFromEnv' "Don't use renderTemplateFromEnv'.  Use renderTemplateFromEnv instead." #-}

renderTemplateFromEnv' :: String -> Q Exp
renderTemplateFromEnv' filename = do
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
    [e| \envObj ->
          let envObjWithErrAndMsg = addErrorsAndMessages envObj
          in case $(pure eitherRenderLambdaExp) envObjWithErrAndMsg of
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

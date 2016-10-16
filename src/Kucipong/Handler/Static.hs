{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Static where

import           Kucipong.Prelude
import           Language.Haskell.TH        (Exp, Q)
import           Language.Haskell.TH.Syntax (addDependentFile, runIO)
import           System.FilePath.Glob       (glob)
import           System.FilePath.Posix      (takeExtension, takeFileName)
import           Web.Spock                  (bytes, setHeader, static)
import           Web.Spock.Core             (get)

staticComponent' :: Q Exp
staticComponent' = do
    files <- runIO $ glob "frontend/dist/static/*"
    mapM_ addDependentFile files
    fileContents <- forM files $ \fpath ->
        liftIO . handleFileContent fpath . readFile $ fpath
    -- TODO Appropreate content type
    -- setHeader "Content-Type" "text/html; charset=utf-8"
    [e| forM_ fileContents $ \(fp, content) ->
        get (static $ takeFileName fp) $
          let
            contentType = case takeExtension fp of
                ".css" -> Just "text/css; charset=utf-8"
                ".gif" -> Just "image/gif"
                ".js" -> Just "application/javascript; charset=utf-8"
                ".jpeg" -> Just "image/jpeg"
                ".jpg" -> Just "image/jpeg"
                ".png" -> Just "image/png"
                ".svg" -> Just "image/svg+xml; charset=utf-8"
                _ -> Nothing
          in do
            maybe (pure ()) (setHeader "Content-Type") contentType
            bytes $ encodeUtf8 content |]
  where
    handleFileContent :: FilePath -> IO Text -> IO (FilePath, Text)
    handleFileContent fp a =
        catch (a >>= \v -> pure (fp, v)) $ \e -> fail $
            "exception occured when trying to parse the static file \"" <>
            fp <> ":\n" <> show (e :: SomeException)

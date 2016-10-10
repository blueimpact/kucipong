{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Static where

import           Kucipong.Prelude
import           Language.Haskell.TH        (Exp, Q)
import           Language.Haskell.TH.Syntax (addDependentFile, runIO)
import           System.FilePath.Glob       (glob)
import           System.FilePath.Posix      (takeFileName)
import           Web.Spock                  (bytes, static)
import           Web.Spock.Core             (get)

staticComponent' :: Q Exp
staticComponent' = do
    files <- runIO $ glob "frontend/dist/static/*"
    mapM_ addDependentFile files
    eitherFileContents <- forM files $ \fpath ->
        liftIO . handleFileContent fpath . readFile $ fpath
    let fileContents = catMaybes eitherFileContents
    -- TODO Appropreate content type
    -- setHeader "Content-Type" "text/html; charset=utf-8"
    [e| forM_ fileContents $ \(fp, content) ->
        get (static $ takeFileName fp) $
            bytes $ encodeUtf8 content |]
  where
    handleFileContent :: FilePath -> IO Text -> IO (Maybe (FilePath, Text))
    handleFileContent fp a =
        catch (a >>= \v -> return (Just (fp, v))) $ \e -> do
            fail $
                "exception occured when trying to parse the static file \"" <>
                fp <> ":\n" <> show (e :: SomeException)
            pure Nothing


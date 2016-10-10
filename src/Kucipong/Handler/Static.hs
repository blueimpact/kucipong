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
    let (x:fileContents) = catMaybes eitherFileContents
    setStaticContent x
  where
    handleFileContent :: FilePath -> IO Text -> IO (Maybe (FilePath, Text))
    handleFileContent fp a =
        catch (a >>= \v -> return (Just (fp, v))) $ \e -> do
            fail $
                "exception occured when trying to parse the static file \"" <>
                fp <> ":\n" <> show (e :: SomeException)
            pure Nothing
    setStaticContent :: (FilePath, Text) -> Q Exp
    setStaticContent (fp, content) =
        [e| get (static $ takeFileName fp) $ do
            -- TODO Appropreate content type
            -- setHeader "Content-Type" "text/html; charset=utf-8"
            bytes $ encodeUtf8 content
        |]

{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store.Util
  where

import Kucipong.Prelude

import Data.HVect (HVect(..))
import Web.Spock (ActionCtxT, UploadedFile(..), files)

import Kucipong.Db (Image(..))
import Kucipong.Monad (FileUploadError(..), MonadKucipongAws(..))

-- uploadedImageToS3
--   :: forall xs m.
--      (MonadIO m, MonadKucipongAws m)
--   => (forall a. ActionCtxT (HVect xs) m a)
--   -- ^ Error handler for when the @\"image\"@ isn't uploaded.
--   -> (forall a. UploadedFile -> FileUploadError -> ActionCtxT (HVect xs) m a)
--   -- ^ Error handler for when an error occurs in the 'awsS3PutUploadedFile'
--   -- call.
--   -> ActionCtxT (HVect xs) m Image
-- uploadedImageToS3 noImageHandler uploadErrorHandler = do
--   filesHashMap <- files
--   uploadedFile <- fromMaybeM noImageHandler $ lookup "image" filesHashMap
--   eitherS3ImageName <- awsS3PutUploadedFile uploadedFile
--   fromEitherOrM eitherS3ImageName $ uploadErrorHandler uploadedFile

data UploadImgErr = UploadImgErr UploadedFile FileUploadError
  deriving (Show, Typeable)

-- | Upload the @\"image\"@ in 'files' to S3.  If the 'Maybe' 'Text' argument
-- is 'Just', then use that as the 'Image' url and don't try to upload anything
-- to S3.
--
-- The return type of this function is somewhat complex.
--
-- If the 'Maybe' 'Text' argument is 'Just', then that will be converted to an
-- 'Image' and returned as an 'Right' ('Just' 'Image').
--
-- If the 'Maybe' 'Text' argument is 'Nothing', then the following will happen:
--
-- First, look for an uploaded file with a filename of @\"image\".  If it
-- doesn't exist, then return 'Right' 'Nothing'.
--
-- If it does exist, then try to upload it to S3.
--
-- If it was successfully uploaded to S3, then return it's 'Image' url as
-- 'Right' ('Just' 'Image').  If it failed to be uploaded, then return 'Left'
-- 'UploadImgErr'.
uploadImgToS3WithDef
  :: forall xs m.
     (MonadIO m, MonadKucipongAws m)
  => Maybe Text -- ^ Default 'Image' url to use if 'Just'.  Not used if
                -- 'Nothing'.
  -> ActionCtxT (HVect xs) m (Either UploadImgErr (Maybe Image))
uploadImgToS3WithDef (Just image) = pure . Right . Just $ Image image
uploadImgToS3WithDef Nothing = do
  filesHashMap <- files
  case lookup "image" filesHashMap of
    Nothing -> pure $ Right Nothing
    Just uploadedFile -> do
      eitherRes <- awsS3PutUploadedFile uploadedFile
      either
        (pure . Left . UploadImgErr uploadedFile)
        (pure . Right . Just)
        eitherRes

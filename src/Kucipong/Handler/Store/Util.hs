{-# LANGUAGE TemplateHaskell #-}

module Kucipong.Handler.Store.Util
  where

import Kucipong.Prelude

import Control.FromSum (fromEitherOrM, fromMaybeM)
import Data.HVect (HVect(..))
import Web.Spock (ActionCtxT, UploadedFile(..), files)

import Kucipong.Db (Image(..))
import Kucipong.Monad (FileUploadError(..), MonadKucipongAws(..))

uploadedImageToS3
  :: forall xs m.
     (MonadIO m, MonadKucipongAws m)
  => (forall a. ActionCtxT (HVect xs) m a)
  -- ^ Error handler for when the @\"image\"@ isn't uploaded.
  -> (forall a. UploadedFile -> FileUploadError -> ActionCtxT (HVect xs) m a)
  -- ^ Error handler for when an error occurs in the 'awsS3PutUploadedFile'
  -- call.
  -> ActionCtxT (HVect xs) m Image
uploadedImageToS3 noImageHandler uploadErrorHandler = do
  filesHashMap <- files
  uploadedFile <- fromMaybeM noImageHandler $ lookup "image" filesHashMap
  eitherS3ImageName <- awsS3PutUploadedFile uploadedFile
  fromEitherOrM eitherS3ImageName $ uploadErrorHandler uploadedFile

module Kucipong.Handler.Store.Util
  ( uploadImgToS3WithDef
  , UploadImgErr(..)
  ) where

import Kucipong.Prelude

import Data.HVect (HVect(..))
import Web.Spock (ActionCtxT, UploadedFile(..), files)

import Kucipong.Db (Image(..))
import Kucipong.Monad (FileUploadError(..), MonadKucipongAws(..))

data UploadImgErr = UploadImgErr UploadedFile FileUploadError
  deriving (Show, Typeable)

-- | Upload the @\"image\"@ in 'files' to S3.  If the 'Maybe' 'Text' argument
-- is 'Just', then use that as the 'Image' url and don't try to upload anything
-- to S3.
--
-- The return type of this function is somewhat complex.
--
-- If the default 'Image' url argument ('Maybe' 'Text') is 'Nothing', and the
-- user has not uploaded a file called @\"image\"@, then this function will
-- return @'Right' 'Nothing'@.  This indicates that the user doesn\'t want to
-- use an image.
--
-- If the default 'Image' url argument ('Maybe' 'Text') is @'Just' \"\"@, and
-- the user has not uploaded a file called @\"image\"@, then this function will
-- return @'Right' 'Nothing'@.  This also indicates that the user doesn\'t want
-- to use an image.
--
-- If the default 'Image' url argument ('Maybe' 'Text') is @'Just' imageName@
-- (where @imagename@ is anything other than the empty string), and the user
-- has not uploaded a file called @\"image\"@, then this function will return
-- @'Right' ('Just' @imageName@), with the @imageName@ being the default
-- 'Image' url.
--
-- If the user has uploaded a file called @\"image\"@, then this function will
-- try to upload it to S3.  If it fails, it will return @'Left' 'UploadImgErr'@.
-- If it succeeds, it will return @'Right' ('Just' image)@.
-- doesn't exist, then return 'Right' 'Nothing'.
uploadImgToS3WithDef
  :: forall xs m.
     (MonadIO m, MonadKucipongAws m)
  => Maybe Image -- ^ Default 'Image' url.
  -> ActionCtxT (HVect xs) m (Either UploadImgErr (Maybe Image))
uploadImgToS3WithDef maybeDefaultImage = do
  filesHashMap <- files
  let maybeUploadedImage = lookupImage filesHashMap
  print maybeDefaultImage
  print maybeUploadedImage
  print filesHashMap
  handleImages maybeDefaultImage maybeUploadedImage
  where
    handleImages
      :: Maybe Image
      -> Maybe UploadedFile
      -> ActionCtxT (HVect xs) m (Either UploadImgErr (Maybe Image))
    handleImages Nothing Nothing =
      pure $ Right Nothing
    handleImages (Just (Image "")) Nothing =
      pure $ Right Nothing
    handleImages (Just defaultImage) Nothing =
      pure . Right $ Just defaultImage
    handleImages _ (Just uploadedImage) =
      awsS3PutUploadedFile uploadedImage >>=
      either (pure . Left . UploadImgErr uploadedImage) (pure . Right . Just)

-- | Lookup a file called @\"image\"@ in the uploaded files 'HashMap'.
--
-- If there no file called @\"image\"@, then return 'Nothing'.
--
-- >>> lookupImage $ mapFromList []
-- Nothing
--
-- If there is a file called @\"image\"@, but it matches the
-- 'EmptyUploadedFile' pattern, then return 'Nothing'.
--
-- >>> let uf_name = "\"\""
-- >>> let uf_contentType = "application/octet-stream"
-- >>> let uf_tempLocation = ""
-- >>> let uploadedFile = UploadedFile{..}
-- >>> lookupImage $ mapFromList [("image", uploadedFile)]
-- Nothing
--
-- If there is a file called @\"image\"@, and it doesn't match the
-- 'EmptyUploadedfile' pattern (that is, it is a real uploaded file), then
-- return 'Just' of that file.
--
-- >>> let uf_name = "name.jpg"
-- >>> let uf_contentType = "image/jpg"
-- >>> let uf_tempLocation = "somelocation.file"
-- >>> let uploadedFile = UploadedFile{..}
-- >>> lookupImage $ mapFromList [("image", uploadedFile)]
-- Just ...
lookupImage :: HashMap Text UploadedFile -> Maybe UploadedFile
lookupImage filesHashMap = lookup "image" filesHashMap >>= \case
  EmptyUploadedFile -> Nothing
  uploadedfile -> Just uploadedfile

-- | This pattern matches an 'UploadedFile' where the user hasn't actually
-- uploaded a file.
pattern EmptyUploadedFile :: UploadedFile
pattern EmptyUploadedFile <-
  UploadedFile {uf_name = "\"\"", uf_contentType = "application/octet-stream"}

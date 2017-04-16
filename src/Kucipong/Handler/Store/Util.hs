
module Kucipong.Handler.Store.Util where

import Kucipong.Prelude

import Database.Persist (entityVal)

import Kucipong.Db (Image(imageS3Name), Key, Store)
import Kucipong.Monad
       (MonadKucipongAws, MonadKucipongDb, awsImageS3Url, dbFindImage,
        dbFindImageWithStoreKey)

awsUrlFromMaybeImageKey
  :: (MonadKucipongAws m, MonadKucipongDb m)
  => Maybe (Key Image) -> m (Maybe Text)
awsUrlFromMaybeImageKey Nothing = pure Nothing
awsUrlFromMaybeImageKey (Just imageKey) = do
  maybeImageEntity <- dbFindImage imageKey
  let maybeImageName = imageS3Name . entityVal <$> maybeImageEntity
  traverse awsImageS3Url maybeImageName

guardMaybeImageKeyOwnedByStore
  :: MonadKucipongDb m
  => Key Store -> Maybe (Key Image) -> m () -> m ()
guardMaybeImageKeyOwnedByStore _ Nothing _ = pure ()
guardMaybeImageKeyOwnedByStore storeKey (Just imageKey) actionIfBad = do
  maybeImageEntity <- dbFindImageWithStoreKey storeKey imageKey
  case maybeImageEntity of
    Just _ -> pure ()
    Nothing -> actionIfBad


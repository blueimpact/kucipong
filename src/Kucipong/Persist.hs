
module Kucipong.Persist where

import Kucipong.Prelude

import Database.Persist
    ( BaseBackend, Entity(Entity), Key, PersistEntity, PersistEntityBackend, PersistStore, PersistStoreWrite
    , repsert )

repsertEntity
    :: forall backend m record
     . ( MonadIO m
       , PersistEntityBackend record ~ BaseBackend backend
       , PersistEntity record
       , PersistStoreWrite backend
       )
    => Key record -> record -> ReaderT backend m (Entity record)
repsertEntity key val = repsert key val $> Entity key val

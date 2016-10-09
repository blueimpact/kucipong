
module Kucipong.Persist where

import Kucipong.Prelude

import Database.Persist
    ( Entity(Entity), Key, PersistEntity, PersistEntityBackend, PersistStore
    , repsert )

repsertEntity
    :: forall (m :: * -> *) record .
       ( MonadIO m
       , PersistStore (PersistEntityBackend record)
       , PersistEntity record
       )
    => Key record
    -> record
    -> ReaderT (PersistEntityBackend record) m (Entity record)
repsertEntity key val = repsert key val $> Entity key val

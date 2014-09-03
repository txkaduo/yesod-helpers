{-# LANGUAGE GADTs #-}
module Yesod.Helpers.Persist where

import Prelude
import Yesod

import Data.Maybe                           (catMaybes)

-- | select current records, replace them with the supplied new list.
-- Try hard to retain old records that are the same as new ones.
replaceWithList ::
    (PersistEntity val, Eq val
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistQuery m
    ) =>
    [Filter val]
    -> [val]                -- ^ new values
    -> m ([Key val], [Key val])
replaceWithList fts new_ones = do
    old_entities <- selectList fts []
    let to_be_deleted = catMaybes $ flip map old_entities $ \(Entity k v) ->
                            if v `elem` new_ones
                                then Nothing
                                else Just k
    let to_be_inserted = catMaybes $ flip map new_ones $ \v ->
                            if v `elem` map entityVal old_entities
                                then Nothing
                                else Just v
    mapM_ delete to_be_deleted
    new_keys <- mapM insert to_be_inserted
    return $ (new_keys, to_be_deleted)

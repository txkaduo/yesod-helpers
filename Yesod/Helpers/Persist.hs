{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Helpers.Persist where

import Prelude
import Yesod

import Data.Maybe                           (catMaybes)
import Database.Persist.Sql                 (transactionUndo, MonadSqlPersist)
import Control.Monad.Trans.Except           (ExceptT, catchE, throwE)
import Data.List                            ((\\))

import Control.Monad.State.Strict           (StateT)
import qualified Control.Monad.State.Strict as S

import Data.Map.Strict                      (Map)
import qualified Data.Map.Strict            as Map

-- | select current records, replace them with the supplied new list.
-- Try hard to retain old records that are the same as new ones.
insertOrUpdateWithList ::
    (PersistEntity val, Eq val
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistQuery m
    ) =>
    [Filter val]
    -> [val]                -- ^ new values
    -> m ([Key val], [Key val])
                            -- ^ new created keys and keys that are not touched
insertOrUpdateWithList fts new_ones = do
    old_entities <- selectList fts []
    let to_be_deleted = catMaybes $ flip map old_entities $ \(Entity k v) ->
                            if v `elem` new_ones
                                then Nothing
                                else Just k
    let to_be_inserted = catMaybes $ flip map new_ones $ \v ->
                            if v `elem` map entityVal old_entities
                                then Nothing
                                else Just v
    new_keys <- mapM insert to_be_inserted
    return $ (new_keys, to_be_deleted)


-- | like insertOrUpdateWithList, but also delete untouched keys.
replaceWithList ::
    (PersistEntity val, Eq val
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistQuery m
    ) =>
    [Filter val]
    -> [val]                -- ^ new values
    -> m ([Key val], [Key val])
replaceWithList fts new_ones = do
    (new_keys, to_be_deleted) <- insertOrUpdateWithList fts new_ones
    mapM_ delete to_be_deleted
    return (new_keys, to_be_deleted)


-- | Automatically undo transaction when there is error throwE'ed
-- in wrapped function.
undoTransWhenE :: (MonadSqlPersist m) =>
    ExceptT e m a -> ExceptT e m a
undoTransWhenE f = catchE f h
    where
        h err = transactionUndo >> throwE err


-- | wrapped 'get', throwE when record does not exist.
getOrE ::
    ( PersistStore m
    , PersistEntity val
    , PersistEntityBackend val ~ PersistMonadBackend m
    ) =>
    e -> Key val -> ExceptT e m val
getOrE err k = get k >>= maybe (throwE err) return


-- | wrapped 'getBy', throwE when record does not exist.
getByOrE ::
    ( PersistUnique m
    , PersistEntity val
    , PersistEntityBackend val ~ PersistMonadBackend m
    ) =>
    e -> Unique val -> ExceptT e m (Entity val)
getByOrE err k = getBy k >>= maybe (throwE err) return


-- | update or replace a record
insertOrUpdate ::
    ( PersistUnique m, PersistQuery m
    , PersistEntity val
    , PersistEntityBackend val ~ PersistMonadBackend m
    ) =>
    val                 -- ^ new value to insert
    -> [Update val]     -- ^ update commands to exec when old record
                        -- with the same unique key already exists.
    -> m (Either (Key val) (Key val))
                        -- ^ Left : the new created id
                        -- ^ Right: the id of old record that has been updated
insertOrUpdate v updates = do
    insertBy v >>= either (upd . entityKey) (return . Left)
    where
        upd k = update k updates >> return (Right k)


-- | Delete all records matching the given criterion
-- , except those whose keys in a specific list.
deleteWhereExcept ::
    ( PersistEntity val, PersistStore m, PersistQuery m
    , PersistEntityBackend val ~ PersistMonadBackend m
    ) =>
    [Filter val]
    -> [Key val]
    -> m [Key val]
deleteWhereExcept filters keeps = do
    ks <- selectKeysList filters []
    let to_del = ks \\ keeps
    mapM_ delete to_del
    return to_del


type CachedInMap val m = StateT (Map (Key val) val) m

-- | when we don't should that might reselect the same record multiple times,
-- use this function to reduce DB access.
cget ::
    ( PersistStore m, PersistEntity val
    , PersistEntityBackend val ~ PersistMonadBackend m
    , Ord (Key val)
    ) =>
    Key val
    -> CachedInMap val m (Maybe val)
cget k = do
    mv <- S.gets $ Map.lookup k
    case mv of
        Just v -> return $ Just v
        Nothing -> do
            mv2 <- get k
            maybe (return ()) (S.modify . Map.insert k) mv2
            return mv2

cselectList ::
    ( PersistQuery m, PersistEntity val
    , PersistEntityBackend val ~ PersistMonadBackend m
    , Ord (Key val)
    ) =>
    [Filter val] -> [SelectOpt val]
    -> CachedInMap val m [Entity val]
cselectList f o = do
    records <- selectList f o
    mapM_ cput records
    return records

cput ::
    ( PersistStore m, PersistEntity val
    , PersistEntityBackend val ~ PersistMonadBackend m
    , Ord (Key val)
    ) =>
    Entity val -> CachedInMap val m ()
cput (Entity k v) = S.modify $ Map.insert k v

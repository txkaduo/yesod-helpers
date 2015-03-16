{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.Persist where

import Prelude
import Yesod

import Data.Maybe                           (catMaybes)

#if MIN_VERSION_persistent(2, 0, 0)
import Database.Persist.Sql                 (SqlBackend)
#else
import Database.Persist.Sql                 (MonadSqlPersist, Connection)
#endif
import Database.Persist.Sql                 (transactionUndo
                                            , connEscapeName)

import Control.Monad.Trans.Except           (ExceptT, catchE, throwE)

#if MIN_VERSION_persistent(2, 0, 0)
import Control.Monad.Trans.Reader           (ReaderT)
#endif

import Control.Monad                        (forM)
import Data.Text                            (Text)
import qualified Data.Text                  as T
import Data.Monoid                          (mconcat)
import Data.List                            ((\\))
import Data.Conduit                         (Sink, await)

import Control.Monad.State.Strict           (StateT)
import qualified Control.Monad.State.Strict as S

import Data.Map.Strict                      (Map)
import qualified Data.Map.Strict            as Map


type PersistQueryUniqueMonad backend n m =
#if MIN_VERSION_persistent(2, 0, 0)
    ( PersistUnique backend
    , PersistQuery backend
        -- PersistUnique/PersistQuery implies PersistStore
    , m ~ ReaderT backend n
    , MonadIO n
    )
#else
    ( PersistUnique m
    , PersistQuery m
        -- PersistUnique/PersistQuery implies PersistStore
    , backend ~ ()
    , n ~ []
    )
#endif

type IsPersistMonadOf backend n m val =
#if MIN_VERSION_persistent(2, 0, 0)
    ( PersistEntityBackend val ~ backend
    , PersistEntity val
    , m ~ ReaderT backend n
    )
#else
    ( PersistEntityBackend val ~ PersistMonadBackend m
    , PersistEntity val
    , n ~ []
    , backend ~ ()
    )
#endif

type PersistUniqueMonad backend n m =
#if MIN_VERSION_persistent(2, 0, 0)
    ( PersistUnique backend
        -- PersistUnique implies PersistStore
    , m ~ ReaderT backend n
    , MonadIO n
    )
#else
    ( PersistUnique m
        -- PersistUnique implies PersistStore
    , backend ~ ()
    , n ~ []
    )
#endif

type PersistQueryMonad backend n m =
#if MIN_VERSION_persistent(2, 0, 0)
    ( PersistQuery backend
        -- PersistQuery implies PersistStore
    , m ~ ReaderT backend n
    , MonadIO n
    )
#else
    ( PersistQuery m
        -- PersistQuery implies PersistStore
    , backend ~ ()
    , n ~ []
    )
#endif

type PersistStoreMonad backend n m =
#if MIN_VERSION_persistent(2, 0, 0)
    ( PersistStore backend
    , m ~ ReaderT backend n
    , MonadIO n
    )
#else
    ( PersistStore m
    , backend ~ ()
    , n ~ []
    )
#endif

-- | select current records, replace them with the supplied new list.
-- Try hard to retain old records that are the same as new ones.
insertOrUpdateWithList ::
    ( Eq val
    , PersistQueryUniqueMonad backend n m
    , IsPersistMonadOf backend n m val
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
    new_keys <- forM to_be_inserted $ \v -> do
                    insertBy v
                        >>= either
                                (\(Entity k _) -> replace k v >> return k)
                                return
    return $ (new_keys, to_be_deleted)


-- | like insertOrUpdateWithList, but also delete untouched keys.
replaceWithList ::
    (PersistEntity val, Eq val
#if MIN_VERSION_persistent(2, 0, 0)
    , PersistEntityBackend val ~ backend
    , PersistUnique backend
    , PersistQuery backend
    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistUnique m
    , PersistQuery m
#endif
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
undoTransWhenE ::
#if MIN_VERSION_persistent(2, 0, 0)
    (MonadIO m) =>
    ExceptT e (ReaderT SqlBackend m) a
    -> ExceptT e (ReaderT SqlBackend m) a
#else
    (MonadSqlPersist m) => ExceptT e m a -> ExceptT e m a
#endif
undoTransWhenE f = catchE f h
    where
        h err = lift transactionUndo >> throwE err


-- | wrapped 'get', throwE when record does not exist.
getOrE ::
    ( PersistEntity val
#if MIN_VERSION_persistent(2, 0, 0)
    , PersistEntityBackend val ~ backend
    , PersistStore backend
    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistStore m
    , PersistEntityBackend val ~ PersistMonadBackend m
#endif
    ) =>
    e -> Key val -> ExceptT e m val
getOrE err k = lift (get k) >>= maybe (throwE err) return


-- | wrapped 'getBy', throwE when record does not exist.
getByOrE ::
    ( PersistEntity val
#if MIN_VERSION_persistent(2, 0, 0)
    , PersistEntityBackend val ~ backend
    , PersistUnique backend
    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistUnique m
#endif
    ) =>
    e -> Unique val -> ExceptT e m (Entity val)
getByOrE err k = lift (getBy k) >>= maybe (throwE err) return


-- | update or replace a record
insertOrUpdate ::
    ( PersistEntity val
#if MIN_VERSION_persistent(2, 0, 0)
    , PersistEntityBackend val ~ backend
    , PersistUnique backend
    , PersistQuery backend
    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistUnique m, PersistQuery m
#endif
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
    ( PersistEntity val
#if MIN_VERSION_persistent(2, 0, 0)
    , PersistEntityBackend val ~ backend
    , PersistStore backend
    , PersistQuery backend
    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistStore m, PersistQuery m
#endif
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
    ( PersistEntity val
#if MIN_VERSION_persistent(2, 0, 0)
    , PersistEntityBackend val ~ backend
    , PersistStore backend
    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistStore m
#endif
    , Ord (Key val)
    ) =>
    Key val
    -> CachedInMap val m (Maybe val)
cget k = do
    mv <- S.gets $ Map.lookup k
    case mv of
        Just v -> return $ Just v
        Nothing -> do
            mv2 <- lift $ get k
            maybe (return ()) (S.modify . Map.insert k) mv2
            return mv2

cselectList ::
    ( PersistEntity val
#if MIN_VERSION_persistent(2, 0, 0)
    , PersistEntityBackend val ~ backend
    , PersistQuery backend
    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistQuery m
#endif
    , Ord (Key val)
    ) =>
    [Filter val] -> [SelectOpt val]
    -> CachedInMap val m [Entity val]
cselectList f o = do
    records <- lift $ selectList f o
    mapM_ cput records
    return records

cput ::
    ( PersistEntity val
#if MIN_VERSION_persistent(2, 0, 0)
    , PersistEntityBackend val ~ backend
    , PersistStore backend
    , m ~ ReaderT backend n
    , MonadIO n
#else
    , PersistEntityBackend val ~ PersistMonadBackend m
    , PersistStore m
#endif
    , Ord (Key val)
    ) =>
    Entity val -> CachedInMap val m ()
cput (Entity k v) = S.modify $ Map.insert k v


getFieldDBName :: PersistEntity a => EntityField a typ -> DBName
getFieldDBName = fieldDB . persistFieldDef

escFieldDBName :: PersistEntity a =>
#if MIN_VERSION_persistent(2, 0, 0)
    SqlBackend
#else
    Connection
#endif
    -> EntityField a typ -> Text
escFieldDBName conn = connEscapeName conn . getFieldDBName

escEntityDBName :: (PersistEntity a, Monad m) =>
#if MIN_VERSION_persistent(2, 0, 0)
    SqlBackend
#else
    Connection
#endif
    -> m a -> Text
escEntityDBName conn = connEscapeName conn . entityDB . entityDef


sinkEntityAsMap :: Monad m => Sink (Entity a) m (Map (Key a) a)
sinkEntityAsMap = go Map.empty
    where
        go s = do
            mx <- await
            case mx of
                Nothing             -> return s
                Just (Entity k v)   -> go $ Map.insert k v s


-- | escape char that is considered as "special" in string with specified escape char
addEscape :: [Char] -> Char -> String -> String
addEscape   _           _   []      = []
addEscape   specials    esc (x:xs)  =
    let next = x : addEscape specials esc xs
    in if x == esc || x `elem` specials
        then esc : next
        else next

removeEscape :: [Char] -> Char -> String -> String
removeEscape    _        _   []          = []
removeEscape    _        _   [x]         = [x]
removeEscape    specials esc (x1:x2:xs)  =
    if x1 == esc
        then if x2 == esc || x2 `elem` specials
                then x2 : f xs
                else x2 : f xs  -- as tradition
        else x1 : f (x2:xs)
    where
        f = removeEscape specials esc

-- | Sepcial chars used in 'LIKE' pattern in SQL
-- XXX: This does not work for all DBMS
--      MS SQL Server has more special chars than others.
unsafeSpecialsForSqlLike :: [Char]
unsafeSpecialsForSqlLike = "%_"


unsafeEscapeForSqlLike :: String -> String
unsafeEscapeForSqlLike =
    -- XXX: 看 persistent 的代码，目前无法加上 ESCAPE 'x' 语句，
    -- 另一方面，不是所有DBMS都有缺省的转义字符（见Sqlite, Oracle, MS SQL Server）
    -- 还好，PostgreSQL MySQL 都有相同的缺省转义字符 '\'
    addEscape unsafeSpecialsForSqlLike '\\'

unsafeEscapeForSqlLikeT :: Text -> Text
unsafeEscapeForSqlLikeT = T.pack . unsafeEscapeForSqlLike . T.unpack

unsafeRemoveEscapeForSqlLike :: String -> String
unsafeRemoveEscapeForSqlLike = removeEscape unsafeSpecialsForSqlLike '\\'

unsafeRemoveEscapeForSqlLikeT :: Text -> Text
unsafeRemoveEscapeForSqlLikeT = T.pack . unsafeRemoveEscapeForSqlLike . T.unpack

contains :: EntityField v Text -> Text -> Filter v
contains = contains2 id

contains2 :: (PersistField a) =>
    (Text -> a)
    -> EntityField v a -> Text -> Filter v
contains2 conv field val = Filter field
                        (Left $ conv $ mconcat ["%", unsafeEscapeForSqlLikeT val, "%"])
                        (BackendSpecificFilter " LIKE ")

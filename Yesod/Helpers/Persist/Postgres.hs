{-# LANGUAGE ScopedTypeVariables #-}
module Yesod.Helpers.Persist.Postgres where

-- {{{1 imports
import           ClassyPrelude.Yesod hiding (Proxy)
import           Data.Proxy
import           Database.Persist.Sql
import           Control.Monad.Trans.Maybe
-- }}}1


data PgColumnInfo = PgColumnInfo
  { pgColInfoName       :: DBName
  , pgColInfoOrdinalNum :: Int
  }

pgSqlGetTableColumnInfo :: forall record m.
                          ( PersistEntity record, MonadIO m
                          , PersistEntityBackend record ~ SqlBackend
                          )
                        => Proxy record
                        -> SqlPersistT m [PgColumnInfo]
-- {{{1
pgSqlGetTableColumnInfo _ = do
  rows <- rawSql sql [ toPersistValue (unDBName $ tableDBName dummy_rec) ]

  return $
    flip map rows $ \ (Single name, Single num) ->
      PgColumnInfo (DBName name) num

  where
    sql = "SELECT attname, attnum FROM pg_attribute WHERE attrelid = ? :: regclass"

    dummy_rec = error "entity record forced" :: record
-- }}}1


data PgIndexInfo = PgIndexInfo
  { pgIndexInfoName      :: DBName
  , pgIndexInfoIsUnique  :: Bool
  , pgIndexInfoIsPrimary :: Bool
  }


data SomeEntityField record = forall typ. SomeEntityField (EntityField record typ)

pgSqlGetIndexInfoByFields :: forall record m.
                            ( PersistEntity record
                            , PersistEntityBackend record ~ SqlBackend
                            , MonadIO m, MonadLogger m
                            )
                          => [SomeEntityField record]
                          -> SqlPersistT m (Maybe PgIndexInfo)
-- {{{1
pgSqlGetIndexInfoByFields fields = runMaybeT $ do
  col_infos <- lift $ pgSqlGetTableColumnInfo (Proxy :: Proxy record)
  ord_num_list <- forM fields $ \ (SomeEntityField ef) -> do
                    let field_name = fieldDBName ef
                    case find ((field_name ==) . pgColInfoName) col_infos of
                      Just info -> return $ pgColInfoOrdinalNum info

                      Nothing -> do
                        $logError $ "field '" <> unDBName field_name <> "' does not exist."
                        mzero

  let indkey_str = intercalate " " (map tshow ord_num_list)

  rows <- lift $ rawSql
                  "SELECT indexrelid,indisunique,indisprimary FROM pg_index WHERE indrelid = ? :: regclass AND indislive=? AND indkey=?"
                  [ toPersistValue $ unDBName $ tableDBName dummy_rec
                  , toPersistValue True
                  , toPersistValue indkey_str
                  ]

  (Single (PersistDbSpecific idx_oid), Single is_unique, Single is_primary) <- MaybeT $ return $ listToMaybe rows

  rows2 <- lift $ rawSql
                    "SELECT relname FROM pg_class WHERE oid = ?"
                    [ toPersistValue (PersistDbSpecific idx_oid)
                    ]

  Single idx_name <- MaybeT $ return $ listToMaybe rows2

  return $ PgIndexInfo (DBName idx_name) is_unique is_primary
  where
    dummy_rec = error "entity record forced" :: record
-- }}}1


data CreateIndexOpt = CreateIndexUnique
                    | CreateIndexConcurrently
                    deriving (Show, Eq, Ord)

pgSqlCreateIndexInfoByFields :: forall record m opts_t.
                               ( PersistEntity record
                               , PersistEntityBackend record ~ SqlBackend
                               , MonadIO m, MonadLogger m
#if MIN_VERSION_classy_prelude(1, 0, 0)
                               , MonoFoldable opts_t
#else
                               , MonoFoldableEq opts_t
#endif
                               , Element opts_t ~ CreateIndexOpt
                               )
                             => Maybe DBName
                             -> opts_t
                             -> [SomeEntityField record]
                             -> SqlPersistT m ()
-- {{{1
pgSqlCreateIndexInfoByFields m_index_name opts fields = do
  conn <- ask
  table_name <- getTableName dummy_rec
  field_names <- forM fields $ \ (SomeEntityField ef) -> getFieldName ef
  let field_names_sql = intercalate "," field_names

  let sql = intercalate " " $ filter (not . null) $
              [ "CREATE"
              , if CreateIndexUnique `oelem` opts
                   then "UNIQUE"
                   else ""
              , "INDEX"
              , if CreateIndexConcurrently `oelem` opts
                   then "CONCURRENTLY"
                   else ""
              , fromMaybe "" $ connEscapeName conn <$> m_index_name
              , "ON"
              , table_name
              , "("
              , field_names_sql
              , ")"
              ]

  rawExecute sql []
  where
    dummy_rec = error "entity record forced" :: record
-- }}}1


pgSqlCreateIndexInfoByFieldsIfNotExist :: forall record m opts_t.
                                         ( PersistEntity record
                                         , PersistEntityBackend record ~ SqlBackend
                                         , MonadIO m, MonadLogger m
#if MIN_VERSION_classy_prelude(1, 0, 0)
                                         , MonoFoldable opts_t
#else
                                         , MonoFoldableEq opts_t
#endif
                                         , Element opts_t ~ CreateIndexOpt
                                         )
                                       => Maybe DBName
                                       -> opts_t
                                       -> [SomeEntityField record]
                                       -> SqlPersistT m ()
-- {{{1
pgSqlCreateIndexInfoByFieldsIfNotExist m_index_name opts fields = do
  m_info <- pgSqlGetIndexInfoByFields fields
  case m_info of
    Nothing -> pgSqlCreateIndexInfoByFields m_index_name opts fields
    Just info -> do
      case m_index_name of
        Nothing -> return ()
        Just index_name -> do
          let old_name = pgIndexInfoName info

          unless (old_name == index_name) $ do
            $logWarn $ "An index with the same fields exists but with a different name (creation cancelled): " <> unDBName old_name
-- }}}1


pgSqlCreateIndexInfoByFieldsIfNotExist1 :: forall record m opts_t typ1.
                                         ( PersistEntity record
                                         , PersistEntityBackend record ~ SqlBackend
                                         , MonadIO m, MonadLogger m
#if MIN_VERSION_classy_prelude(1, 0, 0)
                                         , MonoFoldable opts_t
#else
                                         , MonoFoldableEq opts_t
#endif
                                         , Element opts_t ~ CreateIndexOpt
                                         )
                                       => Maybe DBName
                                       -> opts_t
                                       -> EntityField record typ1
                                       -> SqlPersistT m ()
-- {{{1
pgSqlCreateIndexInfoByFieldsIfNotExist1 m_index_name opts field1 = do
  pgSqlCreateIndexInfoByFieldsIfNotExist m_index_name opts
    ([SomeEntityField field1] :: [SomeEntityField record])
-- }}}1


pgSqlCreateIndexInfoByFieldsIfNotExist2 :: forall record m opts_t typ1 typ2.
                                         ( PersistEntity record
                                         , PersistEntityBackend record ~ SqlBackend
                                         , MonadIO m, MonadLogger m
#if MIN_VERSION_classy_prelude(1, 0, 0)
                                         , MonoFoldable opts_t
#else
                                         , MonoFoldableEq opts_t
#endif
                                         , Element opts_t ~ CreateIndexOpt
                                         )
                                       => Maybe DBName
                                       -> opts_t
                                       -> EntityField record typ1
                                       -> EntityField record typ2
                                       -> SqlPersistT m ()
-- {{{1
pgSqlCreateIndexInfoByFieldsIfNotExist2 m_index_name opts field1 field2 = do
  pgSqlCreateIndexInfoByFieldsIfNotExist m_index_name opts
    ([ SomeEntityField field1, SomeEntityField field2] :: [SomeEntityField record])
-- }}}1



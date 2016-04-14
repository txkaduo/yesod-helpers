{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yesod.Helpers.PersistDiff where

import Prelude
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT)
import Database.Persist
import Data.List.NonEmpty   (NonEmpty, nonEmpty)
import Data.Maybe
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T


data SomeEntityField record = forall v.
  (Eq v, Show v, PersistField v)
    -- ^ 关于 v 的约束仅仅是为了 diffEntity 及其它相关工具函数
  => SomeEntityField
      (EntityField record v)

instance DiffablePersistEntity record => Eq (SomeEntityField record) where
  (SomeEntityField x) == (SomeEntityField y) = eqEntityField x y


class PersistEntity record => DiffablePersistEntity record where
  -- | 所有字段
  allEntityFields :: [SomeEntityField record]

  valByEntiyField :: EntityField record v
                  -> Maybe (record -> v) -- ^ Nothing 是因为有个别 EntityField 其实不指到 record 里的值

  eqEntityField :: EntityField record v1 -> EntityField record v2 -> Bool


-- | 记录某个字段的变化
data SomeEntityFieldDiff record = forall v.
  (Eq v, Show v, PersistField v)
  => SomeEntityFieldDiff
            (EntityField record v)
            v   -- ^ old
            v   -- ^ new


class DiffablePersistEntity record => PartlyUpdatablePersistEntity record where
  updatableEntityFields :: record   -- ^ current value
                        -> [SomeEntityField record]


-- | like 'upsert', but only update allowed fields
partlyUpsert :: ( MonadIO m, backend ~ PersistEntityBackend record
                , PartlyUpdatablePersistEntity record
                , PersistUnique backend
                 )
             => record
             -> ReaderT backend m
                  (Either
                    (NonEmpty (SomeEntityFieldDiff record), Entity record)
                    (Entity record)
                  )
              -- ^ Left: a list of disallowed update fields

partlyUpsert rec = do
  old_or_k <- insertBy rec
  case old_or_k of
    Right k -> return $ Right $ Entity k rec

    Left old_e@(Entity old_id old_rec) -> do
      let diffs = diffPersistEntity old_rec rec
          disallowed = removeEntityFieldDiffsMatch (updatableEntityFields old_rec) diffs

      case nonEmpty disallowed of
        Just xs -> return $ Left (xs, old_e)
        Nothing -> do
          let mk_update (SomeEntityFieldDiff ef _ nv) = ef =. nv
          new_rec2 <- updateGet old_id $ map mk_update diffs
          return $ Right $ Entity old_id new_rec2



someEntityFieldDiffMatchField :: DiffablePersistEntity record
                              => SomeEntityField record
                              -> SomeEntityFieldDiff record
                              -> Bool
someEntityFieldDiffMatchField (SomeEntityField ef1) (SomeEntityFieldDiff ef2 _ _) =
  eqEntityField ef1 ef2


removeEntityFieldDiffsMatch :: DiffablePersistEntity record
                            => [ SomeEntityField record ]
                            -- ^ 出现在此列表里的字段, 会被删除
                            -> [ SomeEntityFieldDiff record ]
                            -> [ SomeEntityFieldDiff record ]
removeEntityFieldDiffsMatch ef_list = filter f
  where
    f diff = not $ any (flip someEntityFieldDiffMatchField diff) ef_list


diffPersistEntity :: forall r. DiffablePersistEntity r
                  => r
                  -> r
                  -> [SomeEntityFieldDiff r]
diffPersistEntity r_old r_new =
  catMaybes $ map diff_field allEntityFields
  where
    diff_field (SomeEntityField ef) = do
      get_v <- m_get_v
      let v_old = get_v r_old
          v_new = get_v r_new

      if v_old == v_new
         then Nothing
         else Just $ SomeEntityFieldDiff ef v_old v_new

      where
        m_get_v = valByEntiyField ef


showPersistEntityDiff :: forall r. PersistEntity r => [SomeEntityFieldDiff r] -> Text
showPersistEntityDiff diffs =
  T.intercalate " "
    [ name
    , "{"
    , T.intercalate ", " $ map show_diff diffs
    , "}"
    ]
  where
    name = unHaskellName $ entityHaskell $ entityDef (Nothing :: Maybe r)

    show_diff (SomeEntityFieldDiff ef v_old v_new) =
      mconcat
        [ unHaskellName $ fieldHaskell $ persistFieldDef ef
        , ": "
        , fromString (show v_old)
        , "=>"
        , fromString (show v_new)
        ]


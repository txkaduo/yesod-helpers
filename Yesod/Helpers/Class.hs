{-# LANGUAGE ScopedTypeVariables #-}
module Yesod.Helpers.Class where

import Prelude
import Database.Persist
import Data.Time
import Data.Text (Text)


-- | 更新时间
class HasUpdatedTime a where
  getUpdatedTime :: a -> UTCTime
  setUpdatedTime :: UTCTime -> a -> a

instance HasUpdatedTime a => HasUpdatedTime (Entity  a) where
  getUpdatedTime = getUpdatedTime . entityVal
  setUpdatedTime t (Entity k v) = Entity k (setUpdatedTime t v)


-- | 对象创建时间
class HasCreatedTime a where
  getCreatedTime :: a -> UTCTime

instance HasCreatedTime a => HasCreatedTime (Entity  a) where
  getCreatedTime = getCreatedTime . entityVal


-- | 是否已被标记为删除
class HasDeleted a where
  isDeleted :: a -> Bool
  markDeleted :: Bool -> a -> a


instance HasDeleted a => HasDeleted (Entity  a) where
  isDeleted = isDeleted . entityVal
  markDeleted b (Entity k v) = Entity k (markDeleted b v)


-- | 很多对象都有个用于显示的字串值
class HasDisplayName a where
  getDisplayName :: a -> Text
  setDisplayName :: Text -> a -> a

instance HasDisplayName a => HasDisplayName (Entity a) where
  getDisplayName = getDisplayName . entityVal
  setDisplayName t (Entity k v) = Entity k (setDisplayName t v)

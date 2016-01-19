{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | 实现: 在 web 服务器处理时，把用户引导到其它网站后，回来继续原来的处理
-- 通常用于处理过程中，发现用户需要登录，登录后可以继续原来的请求
module Yesod.Helpers.ResumeState where

import Prelude
import Data.Proxy
import qualified Data.Map.Strict            as Map
import Data.Map.Strict                      (Map)
import Data.ByteString                      (ByteString)
import qualified Data.ByteString            as B
import Control.Monad.State hiding (forM, mapM)
import Control.Monad.Reader hiding (forM, mapM)
import System.Random                        (randomIO)
import Data.String                          (IsString(..))
import Data.Time
-- import Data.Text                            (Text)
import Data.Acid
import Data.SafeCopy



-- | Some store that can save state
class ReqSaveState a where
    -- | save key of the state
    type ReqSaveKey a :: *
    type ReqSaveVal a :: *

    -- | save the state
    saveNewReqState :: a -> ReqSaveVal a -> IO (ReqSaveKey a)

    -- | lookup a state
    lookupReqState  :: a -> ReqSaveKey a -> IO (Maybe (ByteString, UTCTime))

    -- | delete a state
    dropReqState  :: a -> ReqSaveKey a -> IO ()

    -- | lookup and delete a state
    popReqState  :: a -> ReqSaveKey a -> IO (Maybe (ByteString, UTCTime))

    cleanupReqState :: a -> NominalDiffTime -> IO ()


-- | use this name to pass get parameter, value of which is save state key
savedReqStateParamName :: IsString a => a
savedReqStateParamName = fromString "_SRS"


type family HandlingStateOutput s :: *

type family HandlingStateExtra s :: *

-- | 一个可恢复现场的处理状态
-- 以未登录的请求为例：
-- postXXX 页面: 发现用户未登录，调用 newHandlingState，并保存必要的状态
--               然后重定向至登录页面;
--               如果用户已登录，则直接执行 runHandlingState
--
-- getReturnXXX 页面: 恢复之前保存的状态，执行 runHandlingState
class MonadIO m => HandlingState m s where
    -- type HandlingStatePreCond site s :: *

    -- | 认为 s 总是可以得到的，如果不能取得，或不合法，则可以使用其它短路（如抛异常）
    newHandlingState :: Proxy s -> m s

    resumeHandlingState :: Proxy s -> m s

    runHandlingState :: s -> HandlingStateExtra s -> m (HandlingStateOutput s)


-- | Some simple instance of ReqSaveState
data ReqSaveStateMap = ReqSaveStateMap !(Map ByteString (ByteString, UTCTime))

$(deriveSafeCopy 0 'base ''ReqSaveStateMap)

acidOpSaveReqSaveState :: ByteString -> ByteString -> UTCTime -> Update ReqSaveStateMap Bool
acidOpSaveReqSaveState key val now = do
    ReqSaveStateMap m <- get
    case Map.lookup key m of
        Nothing -> do
                    put $! ReqSaveStateMap $ Map.insert key (val, now) m
                    return True
        Just _  -> return False

acidOpLookupReqSaveState :: ByteString -> Query ReqSaveStateMap (Maybe (ByteString, UTCTime))
acidOpLookupReqSaveState key = do
    ReqSaveStateMap m <- ask
    return $ Map.lookup key m

acidOpDropReqSaveState :: ByteString -> Update ReqSaveStateMap ()
acidOpDropReqSaveState key = do
    ReqSaveStateMap m <- get
    put $! ReqSaveStateMap $ Map.delete key m

acidOpPopReqSaveState :: ByteString -> Update ReqSaveStateMap (Maybe (ByteString, UTCTime))
acidOpPopReqSaveState key = do
    ReqSaveStateMap m <- get
    case Map.lookup key m of
        Nothing -> return Nothing
        Just v -> do
            put $! ReqSaveStateMap $ Map.delete key m
            return $ Just v

acidOpCleanupReqSaveState :: UTCTime -> Update ReqSaveStateMap ()
acidOpCleanupReqSaveState oldest = do
    ReqSaveStateMap m <- get
    put $! ReqSaveStateMap $ Map.filter ((>= oldest) . snd) m

$(makeAcidic ''ReqSaveStateMap
    [ 'acidOpSaveReqSaveState
    , 'acidOpLookupReqSaveState
    , 'acidOpDropReqSaveState
    , 'acidOpPopReqSaveState
    , 'acidOpCleanupReqSaveState
    ]
    )

instance ReqSaveState (AcidState ReqSaveStateMap) where
    type ReqSaveKey (AcidState ReqSaveStateMap) = ByteString
    type ReqSaveVal (AcidState ReqSaveStateMap) = ByteString

    saveNewReqState acid val = go
        where
            go = do
                key <- fmap B.pack $ replicateM 16 randomIO
                now <- getCurrentTime
                done <- update acid $ AcidOpSaveReqSaveState key val now
                if done
                    then return key
                    else go

    lookupReqState acid key = do
        query acid $ AcidOpLookupReqSaveState key

    dropReqState acid key = do
        update acid $ AcidOpDropReqSaveState key

    popReqState acid key = do
        update acid $ AcidOpPopReqSaveState key

    cleanupReqState acid ttl = do
        now <- getCurrentTime
        let oldest = addUTCTime (negate (abs ttl)) now
        update acid $ AcidOpCleanupReqSaveState oldest
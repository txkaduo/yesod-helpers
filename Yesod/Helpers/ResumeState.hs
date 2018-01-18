-- | 实现: 在 web 服务器处理时，把用户引导到其它网站后，回来继续原来的处理
-- 通常用于处理过程中，发现用户需要登录，登录后可以继续原来的请求
module Yesod.Helpers.ResumeState where

import Prelude
-- import Data.Proxy
import qualified Data.Map.Strict            as Map
import Data.Map.Strict                      (Map)
import Data.ByteString                      (ByteString)
import qualified Data.ByteString            as B
import Control.Monad.State hiding (forM, mapM)
import Control.Monad.Reader hiding (forM, mapM)
import System.Random                        (randomIO)
import Data.Default                         (Default(..))
import Data.String                          (IsString(..))
import Control.DeepSeq                      (NFData(..), ($!!))
import Data.Aeson
import Data.Time
import Data.Text                            (Text)
#if defined(VERSION_acid_state)
import Data.Acid
#endif
import Data.SafeCopy

import Yesod.Helpers.Types                  (UrlText)
import Yesod.Helpers.SafeCopy               (SafeCopyJsonVal(..))

-- | Some store that can save state
class ReqSaveState a where
    -- | save key of the state
    type ReqSaveKey a :: *
    type ReqSaveVal a :: *

    -- | save the state
    saveNewReqState :: a -> ReqSaveVal a -> IO (ReqSaveKey a)

    -- | lookup a state
    lookupReqState  :: a -> ReqSaveKey a -> IO (Maybe (ReqSaveVal a, UTCTime))

    -- | delete a state
    dropReqState  :: a -> ReqSaveKey a -> IO ()

    -- | lookup and delete a state
    popReqState  :: a -> ReqSaveKey a -> IO (Maybe (ReqSaveVal a, UTCTime))

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
--
-- 但这个 class 的方法不包含如何从已保存的状态恢复的方法
-- 已不包含怎样取得 HandlingStateExtra 的方法
-- 这些都被认为是其它接口界面的问题
class MonadIO m => HandlingState m s where

    -- | 认为 s 总是可以得到的，如果不能取得，或不合法，则可以使用其它短路（如抛异常）
    newHandlingState :: m s

    -- | 真正处理这个状态，并得到结果
    runHandlingState :: s -> HandlingStateExtra s -> m (HandlingStateOutput s)


-- | 为统一起见，约定 ajax 请求处理时如果遇到需要重定向至另一个页面然后回来再处理的情况
-- 应返回这样的值。并约定其 JSON 格式
-- 典型的情况就是登录之后回来再恢复处理之前暂停的请求
type RetryReason = Text

data RetryOrDone a b = RetryLater UrlText RetryReason a
                            -- ^ 需要完成其它操作后，到所提供的新URL上重试一次，a 是附带的其它数据
                    | JobDone b

instance (ToJSON a, ToJSON b) => ToJSON (RetryOrDone a b) where
    toJSON (RetryLater url reason x)    = object [ "retry" .= object [ "url"    .= url
                                                                     , "reason" .= reason
                                                                     , "data"   .= x
                                                                     ]
                                                 ]
    toJSON (JobDone x)                  = object [ "done" .= x ]


-- | Some simple instance of ReqSaveState
data ReqSaveStateJsonMap = ReqSaveStateJsonMap !(Map ByteString (SafeCopyJsonVal, UTCTime))

$(deriveSafeCopy 0 'base ''ReqSaveStateJsonMap)

instance NFData ReqSaveStateJsonMap where
    rnf (ReqSaveStateJsonMap x) = rnf x `seq` ()

instance Default ReqSaveStateJsonMap where
    def = ReqSaveStateJsonMap def

#if defined(VERSION_acid_state)
acidOpSaveReqSaveState :: ByteString -> SafeCopyJsonVal -> UTCTime -> Update ReqSaveStateJsonMap Bool
acidOpSaveReqSaveState key val now = do
    ReqSaveStateJsonMap m <- get
    case Map.lookup key m of
        Nothing -> do
                    put $!! ReqSaveStateJsonMap $ Map.insert key (val, now) m
                    return True
        Just _  -> return False

acidOpLookupReqSaveState :: ByteString -> Query ReqSaveStateJsonMap (Maybe (SafeCopyJsonVal, UTCTime))
acidOpLookupReqSaveState key = do
    ReqSaveStateJsonMap m <- ask
    return $ Map.lookup key m

acidOpDropReqSaveState :: ByteString -> Update ReqSaveStateJsonMap ()
acidOpDropReqSaveState key = do
    ReqSaveStateJsonMap m <- get
    put $!! ReqSaveStateJsonMap $ Map.delete key m

acidOpPopReqSaveState :: ByteString -> Update ReqSaveStateJsonMap (Maybe (SafeCopyJsonVal, UTCTime))
acidOpPopReqSaveState key = do
    ReqSaveStateJsonMap m <- get
    case Map.lookup key m of
        Nothing -> return Nothing
        Just v -> do
            put $!! ReqSaveStateJsonMap $ Map.delete key m
            return $ Just v

acidOpCleanupReqSaveState :: UTCTime -> Update ReqSaveStateJsonMap ()
acidOpCleanupReqSaveState oldest = do
    ReqSaveStateJsonMap m <- get
    put $!! ReqSaveStateJsonMap $ Map.filter ((>= oldest) . snd) m

$(makeAcidic ''ReqSaveStateJsonMap
    [ 'acidOpSaveReqSaveState
    , 'acidOpLookupReqSaveState
    , 'acidOpDropReqSaveState
    , 'acidOpPopReqSaveState
    , 'acidOpCleanupReqSaveState
    ]
    )

instance ReqSaveState (AcidState ReqSaveStateJsonMap) where
    type ReqSaveKey (AcidState ReqSaveStateJsonMap) = ByteString
    type ReqSaveVal (AcidState ReqSaveStateJsonMap) = SafeCopyJsonVal

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
#endif

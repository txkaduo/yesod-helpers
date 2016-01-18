{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | 实现: 在 web 服务器处理时，把用户引导到其它网站后，回来继续原来的处理
-- 通常用于处理过程中，发现用户需要登录，登录后可以继续原来的请求
module Yesod.Helpers.ResumeState where

import Prelude
import qualified Data.Map.Strict            as Map
import Data.Map.Strict                      (Map)
import Data.ByteString                      (ByteString)
import qualified Data.ByteString            as B
import Control.Monad.State hiding (forM, mapM)
import Control.Monad.Reader hiding (forM, mapM)
import System.Random                        (randomIO)
import Data.String                          (IsString(..))
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
    lookupReqState  :: a -> ReqSaveKey a -> IO (Maybe ByteString)

    -- | delete a state
    dropReqState  :: a -> ReqSaveKey a -> IO ()

    -- | lookup and delete a state
    popReqState  :: a -> ReqSaveKey a -> IO (Maybe ByteString)


-- | use this name to pass get parameter, value of which is save state key
savedReqStateParamName :: IsString a => a
savedReqStateParamName = fromString "_SRS"


{-
class YesodHandlingState site s where
    type YesodHandlingStatePreCond site s :: *
    type YesodHandlingStateOutput site s :: *

    -- | 认为 s 总是可以得到的，如果不能取得，或不合法，则可以使用其它短路（如抛异常）
    newYesodHandlingState :: Proxy s -> HandlerT site IO s

    runYesodHandlingState ::
                        s
                        -> YesodHandlingStatePreCond s
                        -> HandlerT site IO (YesodHandlingStateOutput s)
--}


-- | Some simple instance of ReqSaveState
data ReqSaveStateMap = ReqSaveStateMap !(Map ByteString ByteString)

$(deriveSafeCopy 0 'base ''ReqSaveStateMap)

acidOpSaveReqSaveState :: ByteString -> ByteString -> Update ReqSaveStateMap Bool
acidOpSaveReqSaveState key val = do
    ReqSaveStateMap m <- get
    case Map.lookup key m of
        Nothing -> do
                    put $! ReqSaveStateMap $ Map.insert key val m
                    return True
        Just _  -> return False

acidOpLookupReqSaveState :: ByteString -> Query ReqSaveStateMap (Maybe ByteString)
acidOpLookupReqSaveState key = do
    ReqSaveStateMap m <- ask
    return $ Map.lookup key m

acidOpDropReqSaveState :: ByteString -> Update ReqSaveStateMap ()
acidOpDropReqSaveState key = do
    ReqSaveStateMap m <- get
    put $! ReqSaveStateMap $ Map.delete key m

acidOpPopReqSaveState :: ByteString -> Update ReqSaveStateMap (Maybe ByteString)
acidOpPopReqSaveState key = do
    ReqSaveStateMap m <- get
    case Map.lookup key m of
        Nothing -> return Nothing
        Just v -> do
            put $! ReqSaveStateMap $ Map.delete key m
            return $ Just v

$(makeAcidic ''ReqSaveStateMap
    [ 'acidOpSaveReqSaveState
    , 'acidOpLookupReqSaveState
    , 'acidOpDropReqSaveState
    , 'acidOpPopReqSaveState
    ]
    )

instance ReqSaveState (AcidState ReqSaveStateMap) where
    type ReqSaveKey (AcidState ReqSaveStateMap) = ByteString
    type ReqSaveVal (AcidState ReqSaveStateMap) = ByteString

    saveNewReqState acid val = go
        where
            go = do
                key <- fmap B.pack $ replicateM 16 randomIO
                done <- update acid $ AcidOpSaveReqSaveState key val
                if done
                    then return key
                    else go

    lookupReqState acid key = do
        query acid $ AcidOpLookupReqSaveState key

    dropReqState acid key = do
        update acid $ AcidOpDropReqSaveState key

    popReqState acid key = do
        update acid $ AcidOpPopReqSaveState key

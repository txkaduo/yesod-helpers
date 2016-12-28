module Yesod.Helpers.BgThread where

import ClassyPrelude

import Control.Concurrent.STM               (check)
import Control.Concurrent.Async             (Async, pollSTM, async)
import Control.Monad.Logger
import Control.Monad.Writer.Class           (MonadWriter(..))
import Data.List.NonEmpty                   (NonEmpty(..), nonEmpty)
import System.Timeout                       (timeout)


-- | helper used in devel.hs and main.hs of yesod
loopWatchAsyncList :: (MonadIO m) =>
    STM Bool    -- ^ an action that return True when this loop should be ended
    -> [(a, Async r)]
                -- ^ 'a' is some kind of identification of the action
    -> (a -> Async r -> Either SomeException r -> m ())
                -- ^ callback when a Async finished
    -> m ()
loopWatchAsyncList check_exit a_list0 cb = go a_list0
    where
        go a_list = do
            exit_or_res_list <- liftIO $ atomically $ do
                            need_exit <- check_exit
                            if need_exit
                                then return $ Left ()
                                else do
                                    res_list <- forM a_list $ \(ident, ac) -> do
                                                    m_res <- pollSTM ac
                                                    return ((ident, ac), m_res)
                                    check $ not $ null $ filter (isJust . snd) res_list
                                    return $ Right res_list

            case exit_or_res_list of
                Left _          -> return ()
                Right res_list  -> do
                    a_list' <- liftM catMaybes $ forM res_list $ \((ident, ac), m_res) -> do
                        case m_res of
                            Just res    -> cb ident ac res >> return Nothing
                            Nothing     -> return $ Just (ident, ac)

                    go a_list'


-- | for loopWatchAsyncList
reportBgThreadExited :: (MonadLogger m) =>
    Text -> Async () -> Either SomeException () -> m ()
reportBgThreadExited ident _ay res = do
    case res of
        Left err -> do
            $logError $
                "background thread " <> ident
                <> " exited with error: " <> (fromString $ show err)
        Right _ -> do
            $logWarn $
                "background thread " <> ident <> " exited."


startBgThread :: Text -> IO r -> IO (Text, Async r)
startBgThread ident action = fmap (ident, ) $ async action


startBgThreadW :: (MonadIO m, MonadWriter w m, IsSequence w, Element w ~ (Text, Async r))
               => Text
               -> IO r
               -> m ()
startBgThreadW ident action = do
  liftIO (startBgThread ident action)
    >>= tell . singleton


startBgThreadIdentW :: (MonadIO m, MonadWriter w m, IsSequence w, Element w ~ (Text, Async r))
                    => Text
                    -> (Text -> IO r)
                    -> m ()
startBgThreadIdentW ident action = do
  liftIO (startBgThread ident (action ident)) >>= tell . singleton


-- | 逐一尝试端口，直至列表结束或第一个成功
-- 所执行的函数应该是个长期运行的服务器线程
tryPortToRunAndCheck :: Show a
                     => (LoggingT IO () -> IO())
                     -> Int   -- ^ Seconds to wait to ensure working thread does not exit with exceptions
                     -> NonEmpty a
                     -> Text
                     -> (a -> IO ())
                              -- ^ real work, may never return
                     -> IO ()
tryPortToRunAndCheck run_logging make_sure_alive_seconds ports svc_name f = do
  let report_port ex_mvar port = do
        -- 等待若干秒，如果没收到异常，认为服务器线程成功启动
        m_ex <- timeout (1000 * 1000 * make_sure_alive_seconds) $ readMVar ex_mvar
        run_logging $
          case m_ex of
            Nothing -> do
                $logInfo $ svc_name <> ": running on port: " <> tshow port
            Just ex -> do
                $logWarn $ svc_name <> ": failed to use port " <> tshow port
                          <> ", error was: " <> tshow ex

  let go (port :| other_ports) = do
        ex_mvar <- newEmptyMVar
        void $ fork $ report_port ex_mvar port
        f port `catchIOError`
          \ex -> do
              putMVar ex_mvar ex
              case nonEmpty other_ports of
                Nothing -> do
                  run_logging $
                    $logError $ svc_name <> ": all ports failed."
                  throwIO $ userError $ "Failed to run on any port: " <> unpack svc_name

                Just ps -> go ps

  go ports

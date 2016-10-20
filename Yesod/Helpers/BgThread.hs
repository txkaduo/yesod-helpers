module Yesod.Helpers.BgThread where

import ClassyPrelude

import Control.Concurrent.STM               (check)
import Control.Concurrent.Async             (Async, pollSTM, async)
import Control.Monad.Logger
import Control.Monad.Writer.Class           (MonadWriter(..))


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

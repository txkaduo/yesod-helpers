{-# LANGUAGE ScopedTypeVariables #-}
module Yesod.Helpers.Logger  where

import ClassyPrelude
import Yesod
#if MIN_VERSION_base(4, 13, 0)
import Control.Monad (MonadFail(..))
#endif
import Yesod.Core.Types

import Control.Monad.Trans.Resource
import Conduit
import Data.Default (Default(..))
import qualified Data.Text                  as T

#if !MIN_VERSION_yesod_core(1, 6, 18)
import Control.DeepSeq                      (force)
import Data.Conduit.Combinators             (sourceDirectory)
import Control.Monad.Trans.Resource         (runResourceT)
#endif

import Network.Wai.Logger                   (DateCacheGetter)
import System.FilePath                      (splitFileName, takeFileName)
import System.Directory                     (renameFile)
import qualified Text.Parsec.Number         as PN
import Text.Parsec                          (parse, eof)
import qualified Text.Parsec

#if defined(mingw32_HOST_OS)
import System.PosixCompat.Files                   (getFileStatus, fileSize)
#else
import System.Posix.Files                   (getFileStatus, fileSize)
#endif
import System.Posix.Types                   (COff(..))
import Control.Monad.Logger
import System.Log.FastLogger

#if MIN_VERSION_classy_prelude(1, 5, 0)
import System.IO.Error
#endif

import Data.Aeson
import qualified Data.Aeson.Types           as AT

import Yesod.Helpers.Aeson                  ( parseTextByRead, nullValueToNothing, parseSomeObjects
                                            , parseTextByParsec
                                            )
import Yesod.Helpers.Parsec                 ( parseByteSizeWithUnit )

import Yesod.Compat


type LoggingFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

class LoggingTRunner a where
    runLoggingTWith :: a -> LoggingT m r -> m r


-- | Use a map to implement 'shouldLog' of Yesod class.
-- The logic is the following:
-- 1) Lookup the LogLevel according to the requested LogSource in the map,
--    If found, compare the requested LogLevel to the found one.
--    If not found, go to step 2.
-- 2) repeat step 1, but using a special LogSource "_".
--    If this failed too, the whole process failed.
-- see: shouldLogByLogSourceLevelMap
type LogSourceLevelMap = Map LogSource (Maybe LogLevel)


-- | Use this function to implement 'shouldLog' or 'shouldLogIO'
shouldLogByLogSourceLevelMap :: LogSourceLevelMap -> LogSource -> LogLevel -> Maybe Bool
shouldLogByLogSourceLevelMap the_map source level = do
    fmap (fromMaybe False . fmap (level >=)) $
        lookup source the_map <|> lookup "_" the_map


-- | A helper for reading LogSourceLevelMap from YAML.
-- Example YAML section:
--  min-log-level:
--    SQL     : LevelWarn
--    _       : LevelDebug
parseLogSourceLevelMap :: Value -> AT.Parser LogSourceLevelMap
parseLogSourceLevelMap val = do
    the_map :: HashMap _ _ <- parseJSON val
    fmap mapFromList $ forM (mapToList the_map) $ \(k, v) -> do
        let new_k = fromString k
        new_v <- nullValueToNothing v >>= traverse (parseTextByRead "LogLevel")
        return (new_k, new_v)


logLevelFromText :: Text -> LogLevel
logLevelFromText "debug"        = LevelDebug
logLevelFromText "LevelDebug"   = LevelDebug

logLevelFromText "info"         = LevelInfo
logLevelFromText "LevelInfo"    = LevelInfo

logLevelFromText "warning"      = LevelWarn
logLevelFromText "warn"         = LevelWarn
logLevelFromText "LevelWarn"    = LevelWarn

logLevelFromText "error"        = LevelError
logLevelFromText "LevelError"   = LevelError

logLevelFromText x              = LevelOther x


data LogDest = LogDestFile BufSize FilePath
            | LogDestStdout BufSize
            | LogDestStderr BufSize
            deriving (Eq, Ord, Show)

newLoggerSetByDest :: LogDest -> IO LoggerSet
newLoggerSetByDest (LogDestStdout buf_size)     = newStdoutLoggerSet buf_size
newLoggerSetByDest (LogDestStderr buf_size)     = newStderrLoggerSet buf_size
newLoggerSetByDest (LogDestFile buf_size fp)    = newFileLoggerSet buf_size fp


type LogArchiveAction = IO ()

type ShouldLogPred = LogSource -> LogLevel -> IO Bool

class LogStore a where
    lxPushLogStr  :: a -> LogStr -> IO ()
    lxGetLoggerSet :: a -> LoggerSet

data SomeLogStore = forall h. LogStore h => SomeLogStore h

class ShouldLogPredicator a where
    lxShouldLog     :: a -> ShouldLogPred

data SomeShouldLogPredicator = forall a. ShouldLogPredicator a => SomeShouldLogPredicator a

shouldLogByLevel ::
    LogLevel        -- ^ minimum level
    -> LogLevel     -- ^ level to be tested
    -> Bool
shouldLogByLevel (LevelOther lmin) (LevelOther lv) = lmin == lv
shouldLogByLevel x y = x <= y

shoudLogBySrcLevelMap ::
    HashMap LogSource LogLevel
    -> LogSource
    -> LogLevel
    -> Bool
shoudLogBySrcLevelMap hm src level =
    case lookup src hm <|> lookup "*" hm of
        Nothing         -> False
        Just req_level  -> shouldLogByLevel req_level level

instance ShouldLogPredicator (HashMap LogSource LogLevel) where
    lxShouldLog hm src level = return $ shoudLogBySrcLevelMap hm src level

instance LogStore LoggerSet where
    lxPushLogStr = pushLogStr
    lxGetLoggerSet = id


data LogFileAtMaxSize = LogFileAtMaxSize
                            Int64           -- max size
                            FilePath        -- log file path
                            (IORef Int64)   -- dest. file size, increase when push log
                            LoggerSet

instance LogStore LogFileAtMaxSize where
    lxPushLogStr lf@(LogFileAtMaxSize max_sz _fp size_cnt ls) log_str = do
        lxPushLogStr ls log_str
        new_sz <- atomicModifyIORef' size_cnt $
          \x -> let y = x + fromIntegral (logStrLength log_str) in force (y, y)

        when (new_sz >= max_sz) $ do
            renewLogFileAtMaxSize lf

    lxGetLoggerSet (LogFileAtMaxSize _max_sz _fp _size_cnt ls) = ls


renewLogFileAtMaxSize :: LogFileAtMaxSize -> IO ()
renewLogFileAtMaxSize (LogFileAtMaxSize max_sz fp size_cnt ls) = do
    let renew = do
                flushLogStr ls
                COff fsize <- fileSize <$> getFileStatus fp
                if ( fsize > max_sz )
                    then do
                        cutLogFileThenArchive fp
                        writeIORef size_cnt 0
                        renewLoggerSet ls
                    else
                        writeIORef size_cnt fsize

    renew `catchIOError` annotateRethrowIOError "renewLogFileAtMaxSize" Nothing (Just fp)


-- | create LogFileAtMaxSize
newLogFileAtMaxSize :: Int64 -> BufSize -> FilePath -> IO LogFileAtMaxSize
newLogFileAtMaxSize max_size buf_size fp =
  handle (annotateRethrowIOError "newLogFileAtMaxSize" Nothing (Just fp)) $ do
    logger_set <- newFileLoggerSet buf_size fp
    COff fsize <- fileSize <$> getFileStatus fp

    est_file_sz <- newIORef fsize
    let lf = LogFileAtMaxSize max_size fp est_file_sz logger_set

    when (fsize >= max_size) $ do
        renewLogFileAtMaxSize lf

    return lf


parseSomeLogStoreObj :: Maybe FilePath -> Object -> AT.Parser (IO SomeLogStore)
parseSomeLogStoreObj m_bp o = do
    typ <- o .: "type"
    buf_size <- (o .:? "buf-size"
                    >>= traverse (parseTextByParsec parseByteSizeWithUnit)
                ) .!= defaultBufSize
    case typ of
        "file" -> do
            fp <- fmap (maybe id (</>) m_bp) $ o .: "path"
            m_sz <- o .:? "cut-at-size" >>= traverse (parseTextByParsec parseByteSizeWithUnit)
            case m_sz of
                Nothing -> do
                    return $ do
                        liftM SomeLogStore $
                          newFileLoggerSet buf_size fp
                            `catchIOError` annotateRethrowIOError "newFileLoggerSet" Nothing (Just fp)

                Just sz -> do
                    return $ do
                        liftM SomeLogStore $ newLogFileAtMaxSize sz buf_size fp

        "stdout" -> return $ do
            liftM SomeLogStore $ newStdoutLoggerSet buf_size

        "stderr" -> return $ do
            liftM SomeLogStore $ newStderrLoggerSet buf_size

        _       -> fail $ "unknown handler type: " ++ typ


parseSomeShouldLogPredObj :: Object -> AT.Parser (IO SomeShouldLogPredicator)
parseSomeShouldLogPredObj obj = do
    src_level_map :: HashMap _ _ <- obj .: "src-level" >>= parse_map

    return $ return $ SomeShouldLogPredicator src_level_map

    where
        parse_map = withObject "source-to-level-map" $ \o -> do
                        liftM mapFromList $
                            forM (mapToList o) $ \(k, v) -> do
                                lv <- withText "LogLevel" (return . logLevelFromText) v
                                return (T.strip k, lv)


-- | use this in AppSettings of scaffold site
data LoggerConfig = LoggerConfig
                        (Vector (IO SomeLogStore, IO SomeShouldLogPredicator))
                        (Maybe (IO SomeLogStore, IO SomeShouldLogPredicator))
                            -- ^ default value to use when no others handles the logs

instance Default LoggerConfig where
    def = LoggerConfig mempty Nothing

instance FromJSON LoggerConfig where
    parseJSON = withObject "LoggerConfig" $ \obj -> do
        m_base_path <- obj .:? "base-path"
        LoggerConfig
            <$> (fmap fromList $ obj .: "others" >>= parseSomeObjects "LoggerConfig others" (parse_obj m_base_path))
            <*> (obj .:? "default" >>= traverse (parse_obj m_base_path))
        where
            parse_obj m_bp = \o -> do
                (,) <$> parseSomeLogStoreObj m_bp o
                    <*> parseSomeShouldLogPredObj o


-- | real value for handling all logs
data LogHandlerV = LogHandlerV
                        DateCacheGetter
                        (Vector (SomeLogStore, SomeShouldLogPredicator))
                        (Maybe (SomeLogStore, SomeShouldLogPredicator))

newLogHandlerV :: DateCacheGetter -> LoggerConfig -> IO LogHandlerV
newLogHandlerV getdate (LoggerConfig v m_def) = do
    v2 <- forM v $ uncurry (liftM2 (,))
    m_def2 <- traverse (uncurry (liftM2 (,))) m_def
    return $ LogHandlerV getdate v2 m_def2


-- | use this with runLoggingT
logFuncByHandlerV :: LogHandlerV -> LoggingFunc
logFuncByHandlerV (LogHandlerV getdate v m_def) loc src level msg = do
    log_str_ioref <- newIORef Nothing

    let get_log_str = do
            m_log_str <- readIORef log_str_ioref
            case m_log_str of
                Nothing -> do
                    log_str <- formatLogMessage getdate loc src level msg
                    writeIORef log_str_ioref (Just log_str)
                    return log_str
                Just x -> return x

    not_done <- liftM (null . filter isJust) $ forM v $
        \(SomeLogStore store, SomeShouldLogPredicator p) -> do
            should_log <- lxShouldLog p src level
            if should_log
                then get_log_str >>= lxPushLogStr store >> return (Just ())
                else return Nothing

    when not_done $ do
        void $ forM m_def $ \(SomeLogStore store, SomeShouldLogPredicator p) -> do
            should_log <- lxShouldLog p src level
            if should_log
                then get_log_str >>= lxPushLogStr store >> return (Just ())
                else return Nothing

logFuncFallbackByHandlerV ::
    LogHandlerV
    -> LoggingFunc
logFuncFallbackByHandlerV (LogHandlerV getdate _v m_def) loc src level msg = do
    void $ forM m_def $ \(SomeLogStore store, SomeShouldLogPredicator p) -> do
        should_log <- lxShouldLog p src level
        when ( should_log ) $ do
            formatLogMessage getdate loc src level msg
                >>= lxPushLogStr store

-- | in scaffold site, makeApplication function:
-- we need a LoggerSet to make log middleware.
-- Hence we need to choose a LoggerSet in the available.
chooseYesodLoggerBySrcLV :: LogHandlerV -> LogSource -> IO (Maybe Logger)
chooseYesodLoggerBySrcLV (LogHandlerV getdate v _def_logger_set) src = do
    stores <-
        forM ([LevelDebug, LevelInfo, LevelWarn, LevelError, LevelOther ""]) $ \level -> do
            liftM catMaybes $ forM (toList v) $
                \(store, SomeShouldLogPredicator p) -> do
                    should_log <- lxShouldLog p src level
                    return $ if should_log
                        then Just store
                        else Nothing

    forM (listToMaybe $ join stores) $ \(SomeLogStore store) -> do
        return $ Logger (lxGetLoggerSet store) getdate


defaultYesodLoggerHandlerV :: LogHandlerV -> Maybe Logger
defaultYesodLoggerHandlerV (LogHandlerV getdate _v m_def) =
    flip fmap m_def $ \(SomeLogStore store, _) ->
        Logger (lxGetLoggerSet store) getdate

defaultShouldLogLV :: LogHandlerV -> LogSource -> LogLevel -> IO Bool
defaultShouldLogLV (LogHandlerV _getdate _v m_def) src level =
    liftM (fromMaybe False) $
        forM m_def $ \(_, SomeShouldLogPredicator p) ->
            lxShouldLog p src level


instance LoggingTRunner LogHandlerV where
    runLoggingTWith v = flip runLoggingT (logFuncByHandlerV v)


-- | Used to implement instance of MonadLogger for (HandlerT site m)
monadLoggerLogHandlerV :: (ToLogStr msg)
                       => (site -> LogHandlerV)
                       -> Loc -> LogSource -> LogLevel -> msg -> HandlerOf site ()
monadLoggerLogHandlerV get_logger_v loc src level msg =
#if MIN_VERSION_yesod_core(1, 6, 0)
  HandlerFor $ \ hd ->
#else
  HandlerT $ \ hd ->
#endif
    liftIO $ logFuncByHandlerV (get_logger_v $ rheSite $ handlerEnv hd) loc src level (toLogStr msg)


-- | Used to implement instance of MonadLoggerIO for (HandlerT site m)
askLoggerIoHandlerV :: (site -> LogHandlerV)
                    -> HandlerOf site LoggingFunc
askLoggerIoHandlerV get_logger_v =
#if MIN_VERSION_yesod_core(1, 6, 0)
  HandlerFor $ \ hd ->
#else
  HandlerT $ \ hd ->
#endif
    return $ logFuncByHandlerV (get_logger_v (rheSite $ handlerEnv hd))


localHandlerDataFunc :: LoggingFunc
                     -> (HandlerData a b -> r)
                     -> (HandlerData a b -> r)
localHandlerDataFunc log_func f hd = f hd'
  where rhe  = handlerEnv hd
        rhe' = rhe { rheLog = log_func }
        hd'  = hd { handlerEnv = rhe' }


withLogFuncInHandler :: LoggingFunc
                     -> HandlerOf site a
                     -> HandlerOf site a
#if MIN_VERSION_yesod_core(1, 6, 0)
withLogFuncInHandler log_func (HandlerFor f) = HandlerFor $ 
#else
withLogFuncInHandler log_func (HandlerT f) = HandlerT $
#endif
  localHandlerDataFunc log_func f


withLogFuncInSubHandler :: LoggingFunc
                        -> SubHandlerOf site master a
                        -> SubHandlerOf site master a
#if MIN_VERSION_yesod_core(1, 6, 0)
withLogFuncInSubHandler log_func (SubHandlerFor f) = SubHandlerFor $
#else
withLogFuncInSubHandler log_func (HandlerT f) = HandlerT $
#endif
  localHandlerDataFunc log_func f


-- | usually, in 'HandlerT site m', log will go to the logger returned by
-- 'makeLogger'. With this function, log will be handled by runLoggingTWith
withSiteLogFuncInHandler :: (LoggingTRunner site)
                         => HandlerOf site a
                         -> HandlerOf site a
withSiteLogFuncInHandler h = do
    foundation <- getYesod
    runLoggingTWith foundation $ LoggingT $ \log_func ->
        withLogFuncInHandler log_func h


cutLogFileThenArchive :: FilePath -> IO ()
cutLogFileThenArchive log_path = do
    suf_n <- runResourceT $ runConduit $ sourceDirectory dir_name .| find_next_n (0 :: Int)
    let suf = '.' : show (suf_n + 1 :: Int)
    renameFile log_path (log_path ++ suf)
    where
        (dir_name, log_file_name) = splitFileName log_path

        find_next_n last_n = do
            mx <- await
            case mx of
                Nothing -> return last_n
                Just fp -> do
                    case stripPrefix (log_file_name ++ ".") (takeFileName fp) of
                        Nothing -> find_next_n last_n
                        Just suf -> do
                            case parse parse_suffix "" suf of
                                Left _  -> find_next_n last_n
                                Right x -> find_next_n $ max x last_n

        parse_suffix = do
            x <- PN.nat
            _ <- eof <|> (Text.Parsec.char '.' >> return ())
            return x


-- | To catch IOError rethrow with better message
annotateRethrowIOError :: String
                       -> Maybe Handle
                       -> Maybe FilePath
                       -> IOError
                       -> IO a
annotateRethrowIOError loc m_handle m_fp ex =
  throwIO $ annotateIOError ex loc m_handle m_fp

#if !MIN_VERSION_yesod_core(1, 4, 0)
-- | XXX: copied from source of yesod-core
formatLogMessage :: DateCacheGetter
                 -> Loc
                 -> LogSource
                 -> LogLevel
                 -> LogStr -- ^ message
                 -> IO LogStr
formatLogMessage getdate loc src level msg = do
    now <- getdate
    return $
        toLogStr now `mappend`
        " [" `mappend`
        (case level of
            LevelOther t -> toLogStr t
            _ -> toLogStr $ drop 5 $ show level) `mappend`
        (if null src
            then mempty
            else "#" `mappend` toLogStr src) `mappend`
        "] " `mappend`
        msg `mappend`
        " @(" `mappend`
        toLogStr (fileLocationToString loc) `mappend`
        ")\n"

-- taken from file-location package
-- turn the TH Loc loaction information into a human readable string
-- leaving out the loc_end parameter
fileLocationToString :: Loc -> String
fileLocationToString loc = (loc_package loc) ++ ':' : (loc_module loc) ++
  ' ' : (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
  where
    line = show . fst . loc_start
    char = show . snd . loc_start
#endif

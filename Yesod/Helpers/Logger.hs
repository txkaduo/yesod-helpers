{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module Yesod.Helpers.Logger  where

import Prelude

import Data.Map.Strict                      (Map)
import qualified Data.Map.Strict            as Map
import qualified Data.HashMap.Strict        as HM
import Data.HashMap.Strict                  (HashMap)
import qualified Data.Vector                as V
import Data.Vector                          (Vector)
import qualified Data.Text                  as T
import Data.Default                         (Default(..))
import Control.Monad hiding (forM)
import Control.Applicative
import Data.IORef
import Data.Monoid
import Data.Traversable                     (traverse, forM)
import Data.String                          (fromString)
import Data.List                            (stripPrefix)
import Data.Maybe
import Network.Wai.Logger                   (DateCacheGetter)
import Yesod.Core.Types
import Language.Haskell.TH.Syntax           (loc_package, loc_module, loc_filename, loc_start)
import System.FilePath                      (splitFileName, takeFileName)
import System.Directory                     (renameFile)
import System.IO                            (hPutStrLn, stderr)
import System.IO.Error                      (catchIOError)
import Data.Int                             (Int64)
import qualified Text.Parsec.Number         as PN
import Text.Parsec                          (parse, eof)
import qualified Text.Parsec
import Data.Conduit
import Data.Conduit.Combinators             (sourceDirectory)
import System.Posix.Files                   (getFileStatus, fileSize)
import System.Posix.Types                   (COff(..))
import Control.Monad.Trans.Resource         (runResourceT)
import Control.Monad.Logger
import Control.Monad.Reader                 (ask)
import System.Log.FastLogger

import Data.Aeson
import qualified Data.Aeson.Types           as AT

import Yesod.Helpers.Aeson                  ( parseTextByRead, nullToNothing, parseSomeObjects
                                            , parseTextByParsec
                                            )
import Yesod.Helpers.Parsec                 ( parseByteSizeWithUnit )


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
        Map.lookup source the_map <|> Map.lookup "_" the_map


-- | A helper for reading LogSourceLevelMap from YAML.
-- Example YAML section:
--  min-log-level:
--    SQL     : LevelWarn
--    _       : LevelDebug
parseLogSourceLevelMap :: Value -> AT.Parser LogSourceLevelMap
parseLogSourceLevelMap val = do
    the_map <- parseJSON val
    fmap Map.fromList $ forM (Map.toList the_map) $ \(k, v) -> do
        let new_k = fromString k
        new_v <- nullToNothing v >>= traverse (parseTextByRead "LogLevel")
        return (new_k, new_v)


logLevelFromText :: T.Text -> LogLevel
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
    case HM.lookup src hm <|> HM.lookup "*" hm of
        Nothing         -> False
        Just req_level  -> shouldLogByLevel req_level level

instance ShouldLogPredicator (HashMap LogSource LogLevel) where
    lxShouldLog hm src level = return $ shoudLogBySrcLevelMap hm src level

instance LogStore LoggerSet where
    lxPushLogStr = pushLogStr
    lxGetLoggerSet = id


data LogFileAtMaxSize = LogFileAtMaxSize
                            Int64           -- ^ max size
                            FilePath        -- ^ log file path
                            (IORef Int64)   -- ^ est. file size, increase when push log
                            LoggerSet

instance LogStore LogFileAtMaxSize where
    lxPushLogStr lf@(LogFileAtMaxSize max_sz _fp size_cnt ls) log_str = do
        lxPushLogStr ls log_str
        new_sz <- atomicModifyIORef' size_cnt $
                        \x -> let y = x + fromIntegral (logStrLength log_str) in (y, y)

        when (new_sz >= max_sz) $ do
            renewLogFileAtMaxSize lf

    lxGetLoggerSet (LogFileAtMaxSize _max_sz _fp _size_cnt ls) = ls


renewLogFileAtMaxSize :: LogFileAtMaxSize -> IO ()
renewLogFileAtMaxSize (LogFileAtMaxSize max_sz fp size_cnt ls) = do
    let renew = do
                COff fsize <- fileSize <$> getFileStatus fp
                when ( fsize > max_sz ) $ do
                    cutLogFileThenArchive fp
                    writeIORef size_cnt 0
                    renewLoggerSet ls

    renew `catchIOError` (\e -> do
        hPutStrLn stderr $ "got exception when renewing log file: " ++ show e)


-- | create LogFileAtMaxSize
newLogFileAtMaxSize :: Int64 -> BufSize -> FilePath -> IO LogFileAtMaxSize
newLogFileAtMaxSize max_size buf_size fp = do
    logger_set <- newFileLoggerSet buf_size fp
    COff fsize <- fileSize <$> getFileStatus fp

    est_file_sz <- newIORef fsize
    let lf = LogFileAtMaxSize max_size fp est_file_sz logger_set

    when (fsize >= max_size) $ do
        renewLogFileAtMaxSize lf

    return lf


parseSomeLogStoreObj :: Object -> AT.Parser (IO SomeLogStore)
parseSomeLogStoreObj o = do
    typ <- o .: "type"
    buf_size <- (o .:? "buf-size"
                    >>= traverse (parseTextByParsec parseByteSizeWithUnit)
                ) .!= defaultBufSize
    case typ of
        "file" -> do
            fp <- o .: "path"
            m_sz <- o .:? "cut-at-size" >>= traverse (parseTextByParsec parseByteSizeWithUnit)
            case m_sz of
                Nothing -> do
                    return $ do
                        liftM SomeLogStore $ newFileLoggerSet buf_size fp

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
    src_level_map <- obj .: "src-level" >>= parse_map

    return $ return $ SomeShouldLogPredicator src_level_map

    where
        parse_map = withObject "source-to-level-map" $ \o -> do
                        liftM HM.fromList $
                            forM (HM.toList o) $ \(k, v) -> do
                                lv <- withText "LogLevel" (return . logLevelFromText) v
                                return (T.strip k, lv)


-- | use this in AppSettings of scaffold site
data LoggerConfig = LoggerConfig
                        (Vector (IO SomeLogStore, IO SomeShouldLogPredicator))
                        (Maybe (IO SomeLogStore, IO SomeShouldLogPredicator))
                            -- ^ default value to use when no others handles the logs

instance Default LoggerConfig where
    def = LoggerConfig V.empty Nothing

instance FromJSON LoggerConfig where
    parseJSON = withObject "LoggerConfig" $ \obj -> do
        LoggerConfig
            <$> (fmap V.fromList $ obj .: "others" >>= parseSomeObjects "LoggerConfig others" parse_obj)
            <*> (obj .:? "default" >>= traverse parse_obj)
        where
            parse_obj = \o -> do
                (,) <$> parseSomeLogStoreObj o
                    <*> parseSomeShouldLogPredObj o


-- | real value for handling all logs
data LogHandlerV = LogHandlerV
                        DateCacheGetter
                        (Vector (SomeLogStore, SomeShouldLogPredicator))
                        (Maybe (SomeLogStore, SomeShouldLogPredicator))

newLogHandlerV :: DateCacheGetter -> LoggerConfig -> IO LogHandlerV
newLogHandlerV getdate (LoggerConfig v m_def) = do
    v2 <- V.forM v $ uncurry (liftM2 (,))
    m_def2 <- traverse (uncurry (liftM2 (,))) m_def
    return $ LogHandlerV getdate v2 m_def2


-- | use this with runLoggingT
logFuncByHandlerV ::
    LogHandlerV
    -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
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

    not_done <- liftM (V.null . V.filter isJust) $ V.forM v $
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
    -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
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
            liftM catMaybes $ forM (V.toList v) $
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


withLogFuncInHandlerT ::
    (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
    -> HandlerT site m a
    -> HandlerT site m a
withLogFuncInHandlerT log_func (HandlerT f) = HandlerT $ \hd -> do
    let rhe  = handlerEnv hd
    let rhe' = rhe { rheLog = log_func }
        hd'  = hd { handlerEnv = rhe' }
    f hd'

-- | usually, in 'HandlerT site m', log will go to the logger returned by
-- 'makeLogger'. With this function, log will be handled by runLoggingTWith
withSiteLogFuncInHandlerT :: (LoggingTRunner site, Monad m) =>
    HandlerT site m a
    -> HandlerT site m a
withSiteLogFuncInHandlerT h = do
    foundation <- ask
    runLoggingTWith foundation $ LoggingT $ \log_func ->
        withLogFuncInHandlerT log_func h


cutLogFileThenArchive :: FilePath -> IO ()
cutLogFileThenArchive log_path = do
    suf_n <- runResourceT $ sourceDirectory dir_name $$ find_next_n (0 :: Int)
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
        (if T.null src
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

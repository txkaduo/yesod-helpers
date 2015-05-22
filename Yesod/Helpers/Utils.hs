{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.Utils  where

import Prelude

import Data.Map.Strict                      (Map)
import qualified Data.Map.Strict            as Map
import qualified Data.HashMap.Strict        as HM
import Data.HashMap.Strict                  (HashMap)
import qualified Data.Vector                as V
import Data.Vector                          (Vector)
import qualified Data.Text                  as T
import Data.Default                         (Default(..))
import Control.Monad
import Control.Applicative
import Data.IORef
import Data.Monoid
import Data.Traversable                     (traverse)
import Data.String                          (fromString)
import Data.List                            (sortBy)
import Data.Ord                             (comparing)
import Data.Maybe
import Network.Wai.Logger                   (DateCacheGetter)
import Yesod.Core.Types                     (Logger(..))
import Language.Haskell.TH.Syntax           (loc_package, loc_module, loc_filename, loc_start)
import Control.Monad.Logger
import System.Log.FastLogger

import Data.Aeson
import qualified Data.Aeson.Types           as AT

import Yesod.Helpers.Aeson                  (parseTextByRead, nullToNothing, parseSomeObjects)


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

instance Default LogDest where
    def = LogDestStderr defaultBufSize

instance FromJSON LogDest where
    parseJSON = withObject "LogDest" parseLogDestObj

parseLogDestObj :: Object -> AT.Parser LogDest
parseLogDestObj o = do
    typ <- o .: "type"
    buf_size <- o .:? "buf-size" .!= defaultBufSize
    case typ of
        "file" -> LogDestFile buf_size <$> o .: "path"
        "stdout" -> return $ LogDestStdout buf_size
        "stderr" -> return $ LogDestStderr buf_size
        _       -> fail $ "unknown log destination: " ++ typ

newLoggerSetByDest :: LogDest -> IO LoggerSet
newLoggerSetByDest (LogDestStdout buf_size)     = newStdoutLoggerSet buf_size
newLoggerSetByDest (LogDestStderr buf_size)     = newStderrLoggerSet buf_size
newLoggerSetByDest (LogDestFile buf_size fp)    = newFileLoggerSet buf_size fp


-- | use this in AppSettings of scaffold site
data LoggerConfig = LoggerConfig
                        (Vector
                            ( LogDest
                            , HashMap LogSource LogLevel
                                -- ^ in this map
                                -- LogSource "*" means "match all"
                                -- LogSource "_" means "fallback"
                                -- see logFuncByLoggerV
                            )
                        )
                        (Maybe (LogDest, HashMap LogSource LogLevel))
                            -- ^ default value to use when no others handles the logs
                    deriving (Show)

instance Default LoggerConfig where
    def = LoggerConfig V.empty Nothing

instance FromJSON LoggerConfig where
    parseJSON = withObject "LoggerConfig" $ \obj -> do
        LoggerConfig
            <$> (fmap V.fromList $ obj .: "others" >>= parseSomeObjects "LoggerConfig others" parse_obj)
            <*> (obj .:? "default" >>= traverse parse_obj)
        where
            parse_obj = \o -> do
                (,) <$> o .: "destination"
                    <*> (o .: "src-level" >>= parse_map)

            parse_map = withObject "source-to-level-map" $ \o -> do
                            liftM HM.fromList $
                                forM (HM.toList o) $ \(k, v) -> do
                                    lv <- withText "LogLevel" (return . logLevelFromText) v
                                    return (T.strip k, lv)


shouldLogByLevel ::
    LogLevel        -- ^ minimum level
    -> LogLevel     -- ^ level to be tested
    -> Bool
shouldLogByLevel (LevelOther lmin) (LevelOther lv) = lmin == lv
shouldLogByLevel x y = x <= y


data LoggerV = LoggerV
                DateCacheGetter
                (Vector (LoggerSet, HashMap LogSource LogLevel))
                (Maybe (LoggerSet, HashMap LogSource LogLevel))
                    -- ^ fallback when no match

newLoggerV :: DateCacheGetter -> LoggerConfig -> IO LoggerV
newLoggerV getdate (LoggerConfig v m_def) = do
    lv <- V.forM v $ \(dest, sm) -> do
        logger_set <- newLoggerSetByDest dest
        return (logger_set, sm)

    fallback <- case m_def of
                    Nothing -> return Nothing
                    Just (dest, sm) -> do
                        logger_set <- newLoggerSetByDest dest
                        return $ Just (logger_set, sm)

    return $ LoggerV getdate lv fallback

-- | use this with runLoggingT
logFuncByLoggerV ::
    LoggerV
    -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
logFuncByLoggerV (LoggerV getdate v def_logger_set) loc src level msg = do
    log_str_ioref <- newIORef Nothing

    let get_log_str = do
            m_log_str <- readIORef log_str_ioref
            case m_log_str of
                Nothing -> do
                    log_str <- formatLogMessage getdate loc src level msg
                    writeIORef log_str_ioref (Just log_str)
                    return log_str
                Just x -> return x

    not_done <- liftM (V.null . V.filter isJust) $ V.forM v $ \(logger_set, hm) -> do
        case HM.lookup src hm <|> HM.lookup "*" hm of
            Nothing -> return Nothing
            Just req_level -> do
                if shouldLogByLevel req_level level
                    then do
                        get_log_str >>= pushLogStr logger_set
                        return $ Just ()
                    else return Nothing

    when not_done $ do
        case def_logger_set of
            Nothing -> return ()
            Just (logger_set, hm) -> do
                let req_level = HM.lookup src hm
                when (fromMaybe False $ fmap (flip shouldLogByLevel level) $ req_level) $ do
                    get_log_str >>= pushLogStr logger_set

logFuncFallbackLoggerV ::
    LoggerV
    -> (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
logFuncFallbackLoggerV (LoggerV getdate _v def_logger_set) loc src level msg = do
    case def_logger_set of
        Nothing -> return ()
        Just (logger_set, hm) -> do
            let req_level = HM.lookup src hm
            when (fromMaybe False $ fmap (flip shouldLogByLevel level) $ req_level) $ do
                formatLogMessage getdate loc src level msg >>= pushLogStr logger_set

-- | in scaffold site, makeApplication function:
-- we need a LoggerSet to make log middleware.
-- Hence we need to choose a LoggerSet in the available.
chooseLoggerBySrcLoggerV :: LoggerV -> LogSource -> Maybe Logger
chooseLoggerBySrcLoggerV (LoggerV getdate v _def_logger_set) src =
    Logger <$> (fmap snd $ listToMaybe $ sortBy (comparing fst) $ catMaybes level_and_ls)
            <*> pure getdate
    where
        level_and_ls = flip map (V.toList v) $ \(logger_set, hm) -> do
                        req_level <- HM.lookup src hm
                        return (req_level, logger_set)

defaultLoggerInLoggerV :: LoggerV -> Maybe Logger
defaultLoggerInLoggerV (LoggerV getdate _v def_logger_set) =
    fmap (\x -> Logger (fst x) getdate) def_logger_set

defaultShouldLogInLoggerV :: LoggerV -> LogSource -> LogLevel -> Bool
defaultShouldLogInLoggerV (LoggerV _getdate _v def_logger_set) src level =
    case def_logger_set of
        Nothing -> False
        Just (_logger_set, hm) ->
            let req_level = HM.lookup src hm
            in fromMaybe False $ fmap (flip shouldLogByLevel level) $ req_level


instance LoggingTRunner LoggerV where
    runLoggingTWith v = flip runLoggingT (logFuncByLoggerV v)


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

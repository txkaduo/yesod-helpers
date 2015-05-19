{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.Utils  where

import Prelude

import Data.Map.Strict                      (Map)
import qualified Data.Map.Strict            as Map
import Control.Monad                        (forM)
import Control.Applicative                  ((<|>))
import Data.Traversable                     (traverse)
import Data.String                          (fromString)
import Data.Maybe                           (fromMaybe)
import Control.Monad.Logger

import Data.Aeson
import qualified Data.Aeson.Types           as AT

import Yesod.Helpers.Aeson                  (parseTextByRead, nullToNothing)


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


class LoggingTRunner a where
    runLoggingTWith :: a -> LoggingT m r -> m r

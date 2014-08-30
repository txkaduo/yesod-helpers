{-# LANGUAGE TemplateHaskell #-}
module Yesod.Helpers.Types where

import Prelude

import Data.Time                            (TimeZone, timeZoneOffsetString, parseTime)
import System.Locale                        (defaultTimeLocale)
import Control.Monad                        (mzero, void)
import Text.Parsec
import Yesod.Helpers.Parsec

newtype XTimeZone = XTimeZone { unXTimeZone :: TimeZone }
                    deriving (Show, Read, Eq, Ord)

instance SimpleStringRep XTimeZone where
    simpleEncode = timeZoneOffsetString . unXTimeZone
    simpleParser = do
        manyTill anyChar (eof <|> void space) >>=
            maybe mzero (return . XTimeZone) . (parseTime defaultTimeLocale "%z")

$(derivePersistFieldS "XTimeZone")
$(deriveJsonS "XTimeZone")

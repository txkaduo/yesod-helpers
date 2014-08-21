{-# LANGUAGE TemplateHaskell #-}
module Yesod.Helpers.Types where

import Prelude
import Yesod                                hiding (parseTime)
import qualified Data.Aeson                 as A
import qualified Data.Text                  as T

import Data.Time                            (TimeZone, timeZoneOffsetString, parseTime)
import System.Locale                        (defaultTimeLocale)
import Control.Monad                        (mzero)

newtype XTimeZone = XTimeZone { unXTimeZone :: TimeZone }
                    deriving (Show, Read, Eq)
derivePersistField "XTimeZone"

instance A.FromJSON XTimeZone where
    parseJSON v = A.withText "String" f v
        where
            f s = maybe mzero (return . XTimeZone) $
                        parseTime defaultTimeLocale "%z" $ T.unpack s

instance A.ToJSON XTimeZone where
    toJSON = A.String . T.pack . timeZoneOffsetString . unXTimeZone


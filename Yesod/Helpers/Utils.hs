{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module Yesod.Helpers.Utils where

import Prelude
import Control.Applicative
import Data.Char
import qualified Data.Text                  as T
import Data.Text                            (Text)
import Data.Time                            ( UTCTime, localTimeToUTC, zonedTimeToUTC, TimeZone, ParseTime
                                            , LocalTime(..), midnight
                                            )
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format                     (defaultTimeLocale)
import Data.Time                            (parseTimeM)
#else
import System.Locale                        (defaultTimeLocale)
import Data.Time                            (parseTime)
#endif
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Monad
import System.Random                        (randomIO)

toHalfWidthEnglishAlpha :: Char -> Char
toHalfWidthEnglishAlpha ch
    | ch >= 'Ａ' && ch <= 'Ｚ'  = chr $ ord 'A' + ord ch - ord 'Ａ'
    | ch >= 'ａ' && ch <= 'ｚ'  = chr $ ord 'a' + ord ch - ord 'ａ'
    | otherwise                 = ch


toHalfWidthDigit :: Char -> Char
toHalfWidthDigit ch
    | ch >= '０' && ch <= '９'  = chr $ ord '0' + ord ch - ord '０'
    | otherwise                 = ch

toHalfWidthEnglishAlphaDigit :: Char -> Char
toHalfWidthEnglishAlphaDigit = toHalfWidthEnglishAlpha . toHalfWidthDigit


emptyTextToNothing :: Text -> Maybe Text
emptyTextToNothing t = if T.null t
                        then Nothing
                        else Just t

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right x) = Right x
mapLeft f (Left x)  = Left (f x)


-- | parse a UTCTime, try the following format
-- 2015-01-02 12:53:46+0800
-- 2015-01-02 12:53:46 (as in the specified time zone)
-- 2015年01月02日12时53分46秒 (as in the specified time zone)
humanParseUTCTime :: TimeZone -> String -> Maybe UTCTime
humanParseUTCTime tz s =
    parse_local "%Y-%m-%d %T"
        <|> parse_local "%Y年%m月%d日%H时%M分%S秒"
        <|> parse_day "%Y-%m-%d"
        <|> parse_day "%Y年%m月%d日"
        <|> parse_zoned "%Y-%m-%d %T%z"
        <|> parse_zoned "%Y-%m-%d %T%Z"
    where
        parse_t :: ParseTime t => String -> Maybe t
        parse_t fmt =
#if MIN_VERSION_time(1,5,0)
                parseTimeM False defaultTimeLocale fmt s
#else
                parseTime defaultTimeLocale fmt s
#endif

        parse_local fmt = fmap (localTimeToUTC tz) $ parse_t fmt
        parse_zoned fmt = fmap zonedTimeToUTC $ parse_t fmt
        parse_day fmt = do
            d <- parse_t fmt
            return $ localTimeToUTC tz $ LocalTime d midnight


randomPick :: MonadIO m => [a] -> m a
randomPick choices = do
    idx' <- liftIO randomIO
    let idx = abs idx' `rem` chlen
    return $ choices !! idx
    where
        chlen = length choices


randomString :: MonadIO m => Int -> [Char] -> m [Char]
randomString len chars = replicateM len (randomPick chars)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Helpers.Utils where

import ClassyPrelude
import Data.Char
import Data.List                            ((!!))
import Data.Time                            ( localTimeToUTC, zonedTimeToUTC, TimeZone, ParseTime
                                            , LocalTime(..), midnight
                                            )
#if MIN_VERSION_time(1,5,0)
import Data.Time                            (parseTimeM)
#else
import Data.Time                            (parseTime)
#endif
import Control.Monad.Logger
import System.Random                        (randomIO)
import Control.Monad.Trans.Control          (MonadBaseControl)
import System.Timeout                       (timeout)
import Network.HTTP.Types                   (parseQueryText, renderQueryText, QueryText)
import Network.URI                          (parseURIReference, uriQuery, uriToString
                                            , parseAbsoluteURI, uriAuthority, uriRegName
                                            )
import qualified Blaze.ByteString.Builder   as BBB
import qualified Data.ByteString.UTF8       as UTF8


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


nullToNothing :: MonoFoldable a => a -> Maybe a
nullToNothing t = if null t
                     then Nothing
                     else Just t

{-# DEPRECATED emptyTextToNothing "use nullToNothing instead" #-}
emptyTextToNothing :: Text -> Maybe Text
emptyTextToNothing = nullToNothing

{-# DEPRECATED mapLeft "use left in Control.Arrow instead" #-}
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


-- | generate random string with 'safe' characters:
-- ASCII alpha and number only, with some characters ('l', 'o') excluded.
randomHumanSafeStr :: MonadIO m => IsString s => Int -> m s
randomHumanSafeStr slen = do
    ws <- liftIO $ replicateM slen $ randomIO
    return $ fromString $ flip map ws $
                    \w -> safe_chars !!
                        (fromIntegral (w :: Word8) `mod` length safe_chars)

    where
        -- length of safe_char is 32
        safe_chars = filter (not . (`elem` ['l', 'o'])) $ ['a' .. 'z'] ++ ['2' .. '9']


-- | Make an empty MVar, supply it to the constructor, construct a message for the Chan,
-- wait for a reply in the MVar.
writeChanWaitMVar :: (MonadIO m)
                    => Chan c           -- ^ the message channel
                    -> (MVar a -> c)    -- ^ the channel message maker
                    -> m a
writeChanWaitMVar ch mk_cmd = do
    mvar <- liftIO newEmptyMVar
    let cmd = mk_cmd mvar
    liftIO $ writeChan ch cmd
    liftIO $ takeMVar mvar


-- | Run monadic computation forever, log and retry when exceptions occurs.
foreverLogExc :: (MonadIO m, MonadLogger m, MonadBaseControl IO m)
                => IO Bool     -- ^ This function should be a blocking op,
                            -- return True if the infinite loop should be aborted.
                -> Int      -- ^ ms
                -> m ()
                -> m ()
foreverLogExc block_check_exit interval f = go
    where
        go = do
            f `catchAny` h
            liftIO (timeout interval block_check_exit)
                >>= maybe go (const $ return ())
        h e = do
            $(logError) $ "Got exception in loop: " <> tshow e


urlUpdateQueryText :: (QueryText -> QueryText)
                    -> String
                    -> Maybe String
urlUpdateQueryText f s = do
    uri <- parseURIReference s
    return $ flip (uriToString id) "" $
        uri { uriQuery = (UTF8.toString $ BBB.toByteString $
                                renderQueryText True $
                                    f $ parseQueryText $ UTF8.fromString $ strip_qm $ uriQuery uri)
                }
    where
        strip_qm ('?':xs)   = xs
        strip_qm xs         = xs


queryTextSetParam :: [(Text, Maybe Text)]
                    -> QueryText
                    -> QueryText
queryTextSetParam = flip go
    where
        go ys []        = ys
        go ys (x:xs)    =   let ys' = filter ((/= fst x) . fst) ys
                            in go (ys' ++ [ x ]) xs


extractHostFromAbsUrl :: String -> Maybe String
extractHostFromAbsUrl uri = do
    puri <- parseAbsoluteURI uri
    auth <- uriAuthority puri
    return (uriRegName auth)


-- | Prepend a dot on a domain unless it already begins with a dot.
domainPrependDot :: (IsString s, EqSequence s) => s -> s
domainPrependDot d = if isPrefixOf "." d
                        then d
                        else "." <> d

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Yesod.Helpers.FuzzyDay where

import Prelude

import Control.Monad
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative                  ((<$>))
import Data.Monoid                          (mconcat)
#endif
import qualified Data.Aeson                 as A
import qualified Text.Parsec.Number         as PN
import Data.Aeson.Types                     (Parser, typeMismatch)
import Data.Scientific                      (floatingOrInteger)
import Data.Time
import Data.Maybe                           (fromMaybe)
import Control.Arrow                        ((***))
import Control.DeepSeq                      (NFData(..))
import Control.DeepSeq.Generics             (genericRnf)
import GHC.Generics                         (Generic)
import Yesod.Helpers.Parsec
import Yesod.Helpers.Aeson                  (parseTextByParsec)
import Yesod.Helpers.SafeCopy
import Text.Parsec


data FuzzyDay = FuzzyDayY Int
                | FuzzyDayYM Int Int
                | FuzzyDayYMD Int Int Int
                deriving (Show, Read, Eq, Generic)

instance NFData FuzzyDay where rnf = genericRnf

$(derivePersistFieldS "FuzzyDay")
$(deriveJsonS "FuzzyDay")
$(derivePathPieceS "FuzzyDay")

$(deriveSafeCopySimpleEncoded ''FuzzyDay)

instance SimpleStringRep FuzzyDay where
    simpleEncode (FuzzyDayY y)          = show y
    simpleEncode (FuzzyDayYM y m)       = mconcat [ show y, "-", show m ]
    simpleEncode (FuzzyDayYMD y m d)    = mconcat [ show y, "-", show m, "-", show d ]

    simpleParser = choice
        [ try $ p_ymd, try $ p_ym, try $ p_y ]
        where
            p_ymd = do
                y <- natural
                _ <- string "-"
                m <- natural
                _ <- string "-"
                d <- natural
                return $ FuzzyDayYMD (fromIntegral y) (fromIntegral m) (fromIntegral d)

            p_ym = do
                y <- natural
                _ <- string "-"
                m <- natural
                return $ FuzzyDayYM (fromIntegral y) (fromIntegral m)

            p_y = do
                y <- natural
                return $ FuzzyDayY (fromIntegral y)


parseFuzzyDayFromJson :: A.Value -> Parser FuzzyDay
parseFuzzyDayFromJson (A.String t)   = parseTextByParsec humanParseFuzzyDay t
parseFuzzyDayFromJson (A.Number num) = do
    case floatingOrInteger num of
        Left ( _ :: Double) -> fail "expecting a integer, but got a floating"
        Right x             -> return $ FuzzyDayY x
parseFuzzyDayFromJson v              = typeMismatch "integer" v


-- | Parse human-readable string, output a FuzzyDay
humanParseFuzzyDay :: CharParser FuzzyDay
humanParseFuzzyDay = do
    y <- p_year
    month_res <- optionMaybe $ try $ do
                        spaces
                        sep <- try $ spaces >> p_sep "年"
                        spaces
                        m <- PN.decimal
                        return (m, sep)

    (month, day) <- case month_res of
        Nothing             -> return (Nothing, Nothing)
        Just (month, sep) -> do
            day <- optionMaybe $ try $ do
                    sep2 <- optionMaybe $ spaces >> p_sep "月"
                    spaces
                    d <- PN.decimal
                    when (not $ fromMaybe True $ cmp_sep sep <$> sep2) $
                        fail $ "sep mismatch: " ++ (show sep) ++ " and " ++ show sep2

                    case sep of
                        Right _ -> space >> string "日" >> return ()
                        _       -> return ()

                    return d

            return (Just month, day)

    case (month, day) of
        (Nothing, Nothing)  -> return $ FuzzyDayY y
        (Just m, Nothing)   -> return $ FuzzyDayYM y m
        (Just m, Just d)    -> return $ FuzzyDayYMD y m d
        (Nothing, Just _)   -> fail "got day field without month field"
    where
        p_year = do
            y <- PN.decimal
            return $
                if y < 40
                    then y + 1900
                    else
                        if y < 100
                            then y + 2000
                            else y
        p_sep x = do
            (try $ fmap Left $ choice
                        [ try $ string "."
                        , try $ string "/"
                        , try $ string "-"
                        ]) <|> (fmap Right $ string x)

        cmp_sep (Left x) (Left y)   = x == y
        cmp_sep (Right _) (Right _) = True
        cmp_sep _ _                 = False


-- | Parse human-readable string
humanParseFuzzyDayRange :: CharParser (FuzzyDay, FuzzyDay)
humanParseFuzzyDayRange = do
    d1 <- humanParseFuzzyDay
    optional $ (try $ spaces >> string "--" >> spaces)
                <|> (try $ spaces >> string "-" >> spaces)
    d2 <- humanParseFuzzyDay
    return (d1, d2)


toFuzzyDay :: Day -> FuzzyDay
toFuzzyDay x = FuzzyDayYMD (fromIntegral y) m d
    where
        (y, m, d) = toGregorian x

fromFuzzyDay :: FuzzyDay -> Day
fromFuzzyDay fd = fromGregorian (fromIntegral y) m d
    where
        (y, m, d) = case fd of
                        FuzzyDayY a -> (a, 6, 31)
                        FuzzyDayYM a b -> (a, b, 15)
                        FuzzyDayYMD a b c -> (a, b, c)

fuzzyDayTimeRange :: TimeZone
                    -> FuzzyDay
                    -> (UTCTime, UTCTime)   -- ^ [begin, end)
fuzzyDayTimeRange tz fd = (to_utc *** to_utc) $
    case fd of
        FuzzyDayY y     -> (to_day y 1 1, to_day (y + 1) 1 1)
        FuzzyDayYM y m  ->  let (y', m') = next_month y m
                            in (to_day y m 1, to_day y' m' 1)
        FuzzyDayYMD y m d -> let day1 = to_day y m d
                             in (day1, addDays 1 day1)
    where
        to_utc = localTimeToUTC tz . (\x -> LocalTime x midnight)
        to_day y m d = fromGregorian (fromIntegral y) m d
        next_month y m = if m >= 12
                            then (y + 1, 1)
                            else (y, m + 1)

data FuzzyAge = FuzzyAgeY Int
                | FuzzyAgeYM Int Int
                | FuzzyAgeYMD Int Int Int
                deriving (Show, Read, Eq)

fuzzyDayDiff :: FuzzyDay -> FuzzyDay -> FuzzyAge
fuzzyDayDiff fd1 fd2 =
    case min (level fd1) (level fd2) of
        1 -> FuzzyAgeY df_y
        2 -> FuzzyAgeYM df_y df_m
        _ -> FuzzyAgeYMD df_y df_m df_d
    where
        level x = case x of
                    FuzzyDayY {} -> 1 :: Int
                    FuzzyDayYM {} -> 2
                    FuzzyDayYMD {} -> 3
        df = diffDays (fromFuzzyDay fd1) (fromFuzzyDay fd2)
        df_y = fromIntegral $ df `quot` 365
        df_m = fromIntegral $ df `rem` 365 `quot` 30
        df_d = fromIntegral $ df `rem` 365 `rem` 30


fuzzyAgeYear :: FuzzyAge -> Int
fuzzyAgeYear (FuzzyAgeY y)          = y
fuzzyAgeYear (FuzzyAgeYM y _)       = y
fuzzyAgeYear (FuzzyAgeYMD y _ _)    = y

{-# LANGUAGE TemplateHaskell #-}
module Yesod.Helpers.FuzzyDay where

import Prelude

import Data.Time                            (toGregorian, fromGregorian, Day, diffDays)
import Data.Monoid                          (mconcat)
import Yesod.Helpers.Parsec
import Yesod.Helpers.SafeCopy
import Text.Parsec


data FuzzyDay = FuzzyDayY Int
                | FuzzyDayYM Int Int
                | FuzzyDayYMD Int Int Int
                deriving (Show, Read, Eq)

$(derivePersistFieldS "FuzzyDay")
$(deriveJsonS "FuzzyDay")

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

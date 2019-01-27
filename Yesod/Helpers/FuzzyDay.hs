{-# LANGUAGE ScopedTypeVariables #-}
module Yesod.Helpers.FuzzyDay where

-- {{{1 imports
import ClassyPrelude.Yesod hiding (try, (<|>), optional)

import qualified Data.Aeson                 as A
import qualified Text.Parsec.Number         as PN
import Data.Aeson.Types                     (Parser, typeMismatch)
import Data.Scientific                      (floatingOrInteger)
import Data.Time
import Control.Arrow                        ((***))
import Control.DeepSeq                      (NFData(..))
import Control.DeepSeq.Generics             (genericRnf)
import GHC.Generics                         (Generic)
import Text.Parsec

import Yesod.Helpers.Parsec
import Yesod.Helpers.Aeson                  (parseTextByParsec)
import Yesod.Helpers.SafeCopy
import Yesod.Helpers.Form
-- }}}1


data FuzzyDay = FuzzyDayY Int
                | FuzzyDayYM Int Int
                | FuzzyDayYMD Int Int Int
                deriving (Show, Read, Eq, Generic)

-- {{{1 instances
instance NFData FuzzyDay where rnf = genericRnf

$(derivePersistFieldS "FuzzyDay")
$(deriveJsonS "FuzzyDay")
$(derivePathPieceS "FuzzyDay")

$(deriveSafeCopySimpleEncoded ''FuzzyDay)

instance SimpleEncode FuzzyDay where
    simpleEncode (FuzzyDayY y)          = show y
    simpleEncode (FuzzyDayYM y m)       = mconcat [ show y, "-", show m ]
    simpleEncode (FuzzyDayYMD y m d)    = mconcat [ show y, "-", show m, "-", show d ]

instance SimpleStringRep FuzzyDay where
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
-- }}}1


parseFuzzyDayFromJson :: A.Value -> Parser FuzzyDay
-- {{{1
parseFuzzyDayFromJson (A.String t)   = parseTextByParsec humanParseFuzzyDay t
parseFuzzyDayFromJson (A.Number num) = do
    case floatingOrInteger num of
        Left ( _ :: Double) -> fail "expecting a integer, but got a floating"
        Right x             -> return $ FuzzyDayY x
parseFuzzyDayFromJson v              = typeMismatch "integer" v
-- }}}1


-- | Parse human-readable string, output a FuzzyDay
humanParseFuzzyDay :: Stream s m Char => ParsecT s u m FuzzyDay
-- {{{1
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
                        [ try $ string "-"
                        , try $ string "－"
                        , try $ string "/"
                        , try $ string "."
                        ]) <|> (fmap Right $ string x)

        cmp_sep (Left x) (Left y)   = x == y
        cmp_sep (Right _) (Right _) = True
        cmp_sep _ _                 = False
-- }}}1


-- | Parse human-readable string
humanParseFuzzyDayRange :: Stream s m Char => ParsecT s u m (FuzzyDay, FuzzyDay)
-- {{{1
humanParseFuzzyDayRange = do
    d1 <- humanParseFuzzyDay
    optional $ (try $ spaces >> string "--" >> spaces)
                <|> (try $ spaces >> string "-" >> spaces)
    d2 <- humanParseFuzzyDay
    return (d1, d2)
-- }}}1


toFuzzyDay :: Day -> FuzzyDay
-- {{{1
toFuzzyDay x = FuzzyDayYMD (fromIntegral y) m d
    where
        (y, m, d) = toGregorian x
-- }}}1

fromFuzzyDay :: FuzzyDay -> Day
-- {{{1
fromFuzzyDay fd = fromGregorian (fromIntegral y) m d
    where
        (y, m, d) = case fd of
                        FuzzyDayY a -> (a, 6, 31)
                        FuzzyDayYM a b -> (a, b, 15)
                        FuzzyDayYMD a b c -> (a, b, c)
-- }}}1


fromFuzzyDayMaybe :: FuzzyDay -> Maybe Day
-- {{{1
fromFuzzyDayMaybe fd =
  case fd of
    FuzzyDayYMD y m d -> Just $ fromGregorian (fromIntegral y) m d
    _ -> Nothing
-- }}}1



-- | FuzzyDay所指范围的最后一日
fromFuzzyDayEnd :: FuzzyDay -> Day
-- {{{1
fromFuzzyDayEnd fd = addDays delta $ fromGregorian (fromIntegral y) m d
  where
    ((y, m, d), delta) = case fd of
                           FuzzyDayY a -> ((a, 12, 31), 0)

                           FuzzyDayYM a b -> case b of
                                               12 -> ((a, b, 31), 0)
                                               _ -> ((a, b + 1, 1), -1)

                           FuzzyDayYMD a b c -> ((a, b, c), 0)
-- }}}1


fuzzyDayDayRange :: FuzzyDay
                 -> (Day, Day)
-- {{{1
fuzzyDayDayRange fd =
    case fd of
        FuzzyDayY y     -> (to_day y 1 1, to_day (y + 1) 1 1)
        FuzzyDayYM y m  ->  let (y', m') = next_month y m
                            in (to_day y m 1, to_day y' m' 1)
        FuzzyDayYMD y m d -> let day1 = to_day y m d
                             in (day1, addDays 1 day1)
    where
        to_day y m d = fromGregorian (fromIntegral y) m d
        next_month y m = if m >= 12
                            then (y + 1, 1)
                            else (y, m + 1)
-- }}}1


fuzzyDayTimeRange :: TimeZone
                  -> FuzzyDay
                  -> (UTCTime, UTCTime)   -- ^ [begin, end)
-- {{{1
fuzzyDayTimeRange tz fd = (to_utc *** to_utc) $ fuzzyDayDayRange fd
    where to_utc = localTimeToUTC tz . (\x -> LocalTime x midnight)
-- }}}1


fuzzyDayField :: ( RenderMessage (HandlerSite m) FormMessage
                 , RenderMessage (HandlerSite m) msg
                 , Monad m
                 )
              => msg
              -> Field m FuzzyDay
-- {{{1
fuzzyDayField err_msg = checkMMap from_str to_str strippedTextField
  where
    to_str = fromString . simpleEncode
    from_str t =
      case parse (humanParseFuzzyDay <* eof) "" t of
        Left _ -> return $ Left $ err_msg
        Right x -> return $ Right x
-- }}}1


data FuzzyAge = FuzzyAgeY Int
                | FuzzyAgeYM Int Int
                | FuzzyAgeYMD Int Int Int
                deriving (Show, Read, Eq)

fuzzyDayDiff :: FuzzyDay -> FuzzyDay -> FuzzyAge
-- {{{1
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
-- }}}1


fuzzyAgeYear :: FuzzyAge -> Int
-- {{{1
fuzzyAgeYear (FuzzyAgeY y)          = y
fuzzyAgeYear (FuzzyAgeYM y _)       = y
fuzzyAgeYear (FuzzyAgeYMD y _ _)    = y
-- }}}1


-- vim: set foldmethod=marker:

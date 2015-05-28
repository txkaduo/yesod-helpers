{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module Yesod.Helpers.Utils where

import Prelude
import Data.Char
import qualified Data.Text                  as T
import Data.Text                            (Text)

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

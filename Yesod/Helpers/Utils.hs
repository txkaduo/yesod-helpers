{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module Yesod.Helpers.Utils
    ( module Yesod.Helpers.Utils
    , module Yesod.Helpers.Logger
    ) where

import Prelude
import Data.Char
import Yesod.Helpers.Logger

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

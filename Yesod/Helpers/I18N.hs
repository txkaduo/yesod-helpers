{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.I18N where

import Prelude
import qualified Data.Text              as T

import Text.Shakespeare.I18N            (Lang, RenderMessage, renderMessage)
import Data.Text                        (Text)
import Data.Maybe                       (listToMaybe, fromMaybe)
import Control.Applicative              ((<|>), empty)

-- | Lookup a list of pairs, of which first part is language code.
-- Languages are matched fuzzily.
-- return the first one that is considered matched with the required language.
fuzzyLookupByLang :: Lang -> [ (Lang, a) ] -> Maybe a
fuzzyLookupByLang lang' lang_datas' =
    match_exact <|> match_master <|> match_master_2
    where
        lang        = T.toLower lang'
        lang_datas  = map (\(x,y) -> (T.toLower x, y)) lang_datas'
        match_exact = lookup lang lang_datas

        -- match "zh-cn" with "zh"
        match_master = listToMaybe $ map snd $ filter ((match lang) . fst) lang_datas
            where
                match l l2 = l2 == major
                    where (major, _) = T.breakOn "-" l

        -- match "zh-cn" or "zh" with the last one like "zh-*"
        match_master_2 = listToMaybe $ map snd $ filter ((match lang) . fst) lang_datas
            where
                match l l2 = major1 == major2
                    where
                        (major1, _) = T.breakOn "-" l
                        (major2, _) = T.breakOn "-" l2


chooseByLang :: [Lang] -> [ (Lang, a) ] -> a
chooseByLang langs lang_datas =
    fromMaybe (snd $ last lang_datas)
        (foldl (<|>) empty $ flip map langs $ flip fuzzyLookupByLang lang_datas)


-- | A helper for implement renderMessage method of RenderMessage.
-- Caller provides a NON-empty list of pairs ordered by preference.
-- Each pair contains:
-- * a language code, e.g. zh
-- * a function to render the message type to Text
-- 
-- the last item in list will used as "match-all" item.
renderMessageByList :: [ (Lang, (master -> message -> Text)) ]
    -> master -> [Lang] -> message -> Text
renderMessageByList func_list = f
    where
        f site []              x = (snd $ last func_list) site x
        f site (lang : others) x = case fuzzyLookupByLang lang func_list of
                                        Just func   -> func site x
                                        Nothing     -> f site others x


-- | A helper for implement renderMessage method of RenderMessage.
-- Forward renderMessage call to another type.
renderMessageForward ::
    (RenderMessage site a, RenderMessage site b, Eq a) =>
    [(a, b)] -> site -> [Lang] -> a -> Text
renderMessageForward lst s langs x =
    case lookup x lst of
        Nothing -> "<<unmatched i18n message>>"
        Just y -> renderMessage s langs y

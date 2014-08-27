module Yesod.Helpers.Aeson where

import Prelude
import Data.Aeson
import qualified Data.Text              as T
import qualified Data.Vector            as V
import qualified Data.Aeson             as A

import Data.Text                        (Text)
import Data.Aeson.Types                 (Parser, typeMismatch)
import Data.List                        (find)

-- | Parse a string (usually a word), with a lookup table.
lookupParseText :: String -> [([Text], a)] -> Text -> Parser a
lookupParseText type_name lst s =
    case find ((s `elem`) . fst) lst of
        Nothing -> fail $ "cannot parse string as '" ++ type_name ++ "': " ++ T.unpack s
        Just (_, x) -> return x

-- | case-insensitive version
lookupParseText' :: String -> [([Text], a)] -> Text -> Parser a
lookupParseText' type_name lst s = lookupParseText type_name
                                    (map (\(x, y) -> (map T.toLower x, y)) lst)
                                    (T.toLower s)

lookupParseValue :: String -> [([Text], a)] -> Value -> Parser a
lookupParseValue type_name lst = withText type_name $ lookupParseText type_name lst


parseWordList :: FromJSON a => String -> Value -> Parser [a]
parseWordList _         (A.Array arr)   = mapM parseJSON $ V.toList arr
parseWordList _         (A.String t)    = mapM parseJSON $ map toJSON $ T.words t
parseWordList type_name v               = typeMismatch type_name v

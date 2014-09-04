module Yesod.Helpers.Aeson where

import Prelude
import Data.Aeson
import qualified Data.Text              as T
import qualified Data.Vector            as V
import qualified Data.Aeson             as A
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString        as B
import qualified Text.Parsec            as PC

import Text.Parsec.Text                 ()
import Data.Text                        (Text)
import Data.Text.Encoding               (encodeUtf8)
import Data.Aeson.Types                 (Parser, typeMismatch, modifyFailure)
import Data.List                        (find)
import Data.ByteString                  (ByteString)
import Text.Parsec                      (ParsecT)
import Data.Functor.Identity            (Identity)


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


-- | parse a list of words or do it after spliting a string of words
parseWordList :: String -> (Value -> Parser a) -> Value -> Parser [a]
parseWordList _         f (A.Array arr)   = mapM f $ V.toList arr
parseWordList _         f (A.String t)    = mapM f $ map toJSON $ T.words t
parseWordList type_name _ v               = typeMismatch type_name v

-- | for FromJSON types
parseWordList' :: FromJSON a => String -> Value -> Parser [a]
parseWordList' = flip parseWordList parseJSON


-- | parse a hex-encoded string
parseHexByteString :: String -> Text -> Parser ByteString
parseHexByteString type_name s = do
    let (good, invalid) = B16.decode $ encodeUtf8 s
    if B.null invalid
        then return good
        else fail $ "cannot parse string as type '" ++ type_name ++ "'"


-- | expecting a Array, parse each value in it
parseArray ::
    String                          -- ^ the type name when reporting error
    -> (Value -> Parser a)          -- ^ function to apply on each of array
    -> Value -> Parser [a]
parseArray type_name f = withArray type_name $  parseList type_name f . V.toList

parseList ::
    String                   -- ^ the type name when reporting error
    -> (Value -> Parser a)         -- ^ function to apply on each of array
    -> [Value] -> Parser [a]
parseList type_name f = mapM f' . zip [0..]
    where
        f' (idx, v) = modifyFailure report $ f v
            where
                report s =
                    "Failed to parse item in array with index="
                        ++ show (idx :: Int)
                        ++ " as expected type '" ++ type_name ++ "': "
                        ++ s


reportExpected :: String -> Parser a -> Parser a
reportExpected expected f = modifyFailure report f
    where
        report s =
            "Failed to parse as expected type '" ++ expected ++ "': " ++ s


-- | parse text value by Parsec parser
parseTextByParsec ::
    ParsecT Text () Identity a
    -> Text -> Parser a
parseTextByParsec p t =
        case PC.parse p "" t of
            Left err -> fail $ show err
            Right x -> return x


-- | like parseWordList, but somewhat more general
parseListByParsec ::
    String                              -- ^ type name for error report
    -> ParsecT Text () Identity b       -- ^ separator parser
    -> ParsecT Text () Identity a       -- ^ item parser
    -> Value -> Parser [a]
parseListByParsec type_name _ p (A.Array arr) =
    parseList
        type_name
        (withText type_name $ reportExpected type_name . parseTextByParsec p)
        (V.toList arr)

parseListByParsec type_name sep p (A.String t) =
    reportExpected type_name $ parseTextByParsec (p `PC.sepBy` sep) t

parseListByParsec type_name _ _ v = typeMismatch type_name v

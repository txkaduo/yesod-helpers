module Yesod.Helpers.Aeson where

import Prelude
import Data.Aeson
import qualified Data.Text              as T
import qualified Data.Vector            as V
import qualified Data.Aeson             as A
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString        as B
import qualified Text.Parsec            as PC
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans              (lift)
import Data.List                        (intersperse)

import Text.Parsec.Text                 ()
import Data.Text                        (Text)
import Data.Text.Encoding               (encodeUtf8)
import Data.Aeson.Types                 (Parser, typeMismatch, modifyFailure)
import Data.List                        (find)
import Data.ByteString                  (ByteString)
import Text.Parsec                      (ParsecT)
import Data.Functor.Identity            (Identity)
import Data.Maybe                       (fromMaybe)

import Yesod.Helpers.Parsec             (splitByParsec)

nullToNothing :: FromJSON a => Maybe Value -> Parser (Maybe a)
nullToNothing Nothing     = return Nothing
nullToNothing (Just Null) = return Nothing
nullToNothing (Just v)    = parseJSON v

(.:?*) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:?* key = obj .:? key >>= nullToNothing

{-
-- | combines (.:) and (>>=) for better error reporting
parseReqSubField :: FromJSON a => Object -> Text -> (a -> Parser b) -> Parser b
parseReqSubField obj field p = obj .: field >>= modifyFailure report . p
    where
        report s = "Failed to parse field '" ++ (T.unpack field) ++ "': " ++ s
-}

type SubFieldParser a = S.StateT [Text] Parser a

-- | run action of Parser but modify its error message to report
-- where the error was in (path of fields)
reportFieldPathParser :: Parser a -> SubFieldParser a
reportFieldPathParser p = do
    fields <- S.get
    let report s =
            if null fields
                then s
                else "in field path '"
                        ++ (T.unpack $ T.concat $ intersperse (T.singleton '/') $
                                reverse fields)
                        ++ "': " ++ s
    lift $ modifyFailure report p

freezeField :: SubFieldParser a -> SubFieldParser a
freezeField p = do
    s <- S.get
    lift $ S.evalStateT p s

withObjectS :: String -> (Object -> SubFieldParser a) -> Value -> SubFieldParser a
withObjectS = liftWithS withObject

liftWithS ::
    (String -> (b -> Parser a) -> Value -> Parser a)
    -> String -> (b -> SubFieldParser a) -> Value -> SubFieldParser a
liftWithS wf expected p v = do
    ks <- S.get
    lift $ wf expected (flip S.evalStateT ks . p) v


atField :: FromJSON a =>
    (Object -> Text -> Parser a)
    -> Object
    -> Text
    -> (a -> Parser b)
    -> SubFieldParser b
atField getf obj key p = do
    ks <- S.get
    lift $ S.evalStateT (reportFieldPathParser $ getf obj key >>= p) (key : ks)

atFieldS :: FromJSON a =>
    (Object -> Text -> Parser a)
    -> Object
    -> Text
    -> (a -> SubFieldParser b)
    -> SubFieldParser b
atFieldS getf obj key p = do
    ks <- S.get
    lift $ S.evalStateT ((reportFieldPathParser $ getf obj key) >>= p) (key : ks)

reqField :: FromJSON a => Object -> Text -> (a -> Parser b) -> SubFieldParser b
reqField = atField (.:)

reqFieldS :: FromJSON a => Object -> Text -> (a -> SubFieldParser b) -> SubFieldParser b
reqFieldS = atFieldS (.:)

optField :: FromJSON a => Object -> Text -> (Maybe a -> Parser b) -> SubFieldParser b
optField obj key p = do
    ks <- S.get
    lift $ S.evalStateT (reportFieldPathParser $ obj .:? key >>= p) (key : ks)

optFieldS :: FromJSON a => Object -> Text -> (Maybe a -> SubFieldParser b) -> SubFieldParser b
optFieldS obj key p = do
    ks <- S.get
    lift $ S.evalStateT ((reportFieldPathParser $ obj .:? key) >>= p) (key : ks)

(+.:) :: (FromJSON a) => Object -> Text -> SubFieldParser a
obj +.: key = atField (.:) obj key return

(+.:?) :: (FromJSON a) => Object -> Text -> SubFieldParser (Maybe a)
obj +.:? key = atField (.:?) obj key return

(+.:?*) :: (FromJSON a) => Object -> Text -> SubFieldParser (Maybe a)
obj +.:?* key = atField (.:?*) obj key return

(+.!=) :: SubFieldParser (Maybe a) -> a -> SubFieldParser a
p +.!= x = fmap (fromMaybe x) p

runSubFieldParser :: [Text] -> SubFieldParser a -> Parser a
runSubFieldParser ks sp = S.evalStateT sp $ reverse ks

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


-- | parse a list or after spliting the text into a list
-- use a Parsec parser to split the the text
parseListSepParsec ::
    String
    -> ParsecT Text () Identity b   -- ^ separator parser
    -> (Value -> Parser a)          -- ^ parse each value of the list
                                    -- the value maybe a String if it is
                                    -- from spliting a long text,
                                    -- otherwise it is the value in an array
    -> Value -> Parser [a]
parseListSepParsec _            _   f (A.Array arr)     = mapM f $ V.toList arr
parseListSepParsec type_name    sep f (A.String t)      =
    mapM f $ map toJSON $
        case splitByParsec sep t of
            Left err -> fail $ "when parsing expected type '" ++
                                type_name ++ "': " ++ err
            Right x -> return x

parseListSepParsec type_name    _   _ v                 = typeMismatch type_name v


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

parseArrayS ::
    String                          -- ^ the type name when reporting error
    -> (Value -> SubFieldParser a)          -- ^ function to apply on each of array
    -> Value -> SubFieldParser [a]
parseArrayS type_name f = liftWithS withArray type_name $ parseListS f . V.toList

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
parseListS ::
    (Value -> SubFieldParser a)         -- ^ function to apply on each of array
    -> [Value] -> SubFieldParser [a]
parseListS f = mapM f' . zip [(0::Int)..]
    where
        f' (idx, v) = do
            ks <- S.get
            let new_key = T.pack $ concat $ [ "[", show idx, "]" ]
            lift $ S.evalStateT (f v) (new_key : ks)

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

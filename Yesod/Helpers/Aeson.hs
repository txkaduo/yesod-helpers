{-# LANGUAGE ScopedTypeVariables #-}
module Yesod.Helpers.Aeson where

-- {{{1
import ClassyPrelude
#if MIN_VERSION_base(4, 13, 0)
import Control.Monad (MonadFail(..))
#else
import Data.Functor.Identity            (Identity)
#endif

import Prelude (ReadS, readsPrec)
import Data.Aeson
import qualified Data.Text              as T
import qualified Data.Vector            as V
import qualified Data.Aeson             as A
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Text.Parsec            as PC
import qualified Control.Monad.Trans.State as S

import Text.Parsec.Text                 ()
import Data.Aeson.Types                 (Parser, typeMismatch, modifyFailure)
import Text.Parsec                      (ParsecT)

import Data.Scientific                  (floatingOrInteger)
import Text.ParserCombinators.ReadP     (ReadP, readP_to_S)

import Yesod.Helpers.Parsec             (splitByParsec)
import Yesod.Helpers.Utils              (nullToNothing)
-- }}}1

nullValueToNothing :: FromJSON a => Maybe Value -> Parser (Maybe a)
nullValueToNothing Nothing     = return Nothing
nullValueToNothing (Just Null) = return Nothing
nullValueToNothing (Just v)    = parseJSON v


-- | convert the value inside json that can be suitable to be considered as null, to Nothing
nullTextToNothing :: Maybe Value -> Maybe Value
nullTextToNothing Nothing               = Nothing
nullTextToNothing (Just Null)           = Nothing
nullTextToNothing (Just (A.String t))   = fmap A.String $ nullToNothing t
nullTextToNothing x                     = x


(.:?*) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:?* key = obj .:? key >>= nullValueToNothing

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


atField :: (Object -> Text -> Parser a)
        -> Object
        -> Text
        -> (a -> Parser b)
        -> SubFieldParser b
atField getf obj key p = do
    ks <- S.get
    lift $ S.evalStateT (reportFieldPathParser $ getf obj key >>= p) (key : ks)

atFieldS :: (Object -> Text -> Parser a)
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
  case B16.decodeBase16 $ encodeUtf8 s of
    Left err -> fail $ "cannot parse hex-string as type '" ++ type_name ++ "': " <> T.unpack err
    Right good -> pure good


-- | parse a base64-encoded string
parseBase64ByteString :: String -> Text -> Parser ByteString
parseBase64ByteString type_name s = do
    case B64.decodeBase64 $ encodeUtf8 s of
        Left err    -> fail $ "cannot parse base64-string as type '"
                                ++ type_name ++ "': " ++ T.unpack err
        Right good  -> return good

parseUrlBase64ByteString :: String -> Text -> Parser ByteString
parseUrlBase64ByteString type_name s = do
    case B64U.decodeBase64 $ encodeUtf8 s of
        Left err    -> fail $ "cannot parse url-base64-string as type '"
                                ++ type_name ++ "': " ++ T.unpack err
        Right good  -> return good

newtype Base64EncodedByteString = Base64EncodedByteString { unBase64EncodedByteString :: ByteString }
                                deriving (Eq, Ord)
instance FromJSON Base64EncodedByteString where
    parseJSON v = fmap Base64EncodedByteString $
                    parseJSON v >>= parseBase64ByteString "Base64EncodedByteString"

instance ToJSON Base64EncodedByteString where
    toJSON = toJSON . B64.encodeBase64 . unBase64EncodedByteString

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

parseSomeObjects ::
    String
    -> (Object -> Parser a)
    -> Value
    -> Parser [a]
parseSomeObjects type_name  f (A.Array arr)     = mapM (withObject type_name f) $ V.toList arr
parseSomeObjects _          f (A.Object obj)    = fmap return $ f obj
parseSomeObjects type_name  _ v                 = typeMismatch type_name v


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
#if MIN_VERSION_base(4, 13, 0)
    (MonadFail m)
#else
    (Monad m)
#endif
    => ParsecT Text () Identity a
    -> Text -> m a
parseTextByParsec p t =
        case PC.parse p "" t of
            Left err -> fail $ show err
            Right x -> return x


-- | use a ReadS to parse text value
parseTextByReadS :: String -> ReadS a -> Text -> Parser a
parseTextByReadS type_name rs t = do
    case listToMaybe $ sort_result $ rs $ T.unpack t of
        Nothing         -> fail $ "faied to parse as type " ++ type_name
                            ++ " from string: " ++ t'
        Just (x, left)  -> do
                            if null left
                                then return x
                                else fail $ "faied to parse as type " ++ type_name
                                    ++ ": unexpected extra string " ++ show left
    where
        sort_result = sortWith (length . snd)
        t' = T.unpack t

-- | use a ReadP to parse text value
parseTextByReadP :: String -> ReadP a -> Text -> Parser a
parseTextByReadP type_name rp t = do
    parseTextByReadS type_name (readP_to_S rp) t

parseTextByRead :: Read a => String -> Text -> Parser a
parseTextByRead type_name t = do
    parseTextByReadS type_name (readsPrec 0) t

-- | Parse text or number value by coercing input as Text
parseNumOrText :: (Text -> Parser a) -> A.Value -> Parser a
parseNumOrText p (A.String t)   = p t
parseNumOrText p (A.Number num) = do
    case floatingOrInteger num of
        Left (x :: Double)      -> p $ T.pack $ show x
        Right (x :: Integer)    -> p $ T.pack $ show x
parseNumOrText _ v              = typeMismatch "text" v

-- | Parse an integer or Text, into a integer
parseIntWithTextparsec :: Integral a =>
    ParsecT Text () Identity a
    -> Value
    -> Parser a
parseIntWithTextparsec p (A.String t)   = parseTextByParsec p t
parseIntWithTextparsec _ (A.Number num) = do
    case floatingOrInteger num of
        Left ( _ :: Double) -> fail "expecting a integer, but got a floating"
        Right x            -> return x
parseIntWithTextparsec _ v              = typeMismatch "integer" v

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


-- | parse a subfield: required a text, strip it before return
reqStripNonEmptyText :: Object -> Text -> SubFieldParser Text
reqStripNonEmptyText obj name = do
    atField (.:) obj name $ \x' -> do
        let x = T.strip x'
        when (T.null x) $ fail $ (T.unpack name) ++ " cannot be empty"
        return x


-- | parse a subfield: optional text, strip it before return
optStripNonEmptyText :: Object -> Text -> SubFieldParser (Maybe Text)
optStripNonEmptyText obj name = do
    atField (.:?*) obj name $ \x' -> do
        let x = fmap T.strip x'
        when (maybe False T.null x) $ fail $ (T.unpack name) ++ " cannot be empty"
        return x




-- vim: set foldmethod=marker:

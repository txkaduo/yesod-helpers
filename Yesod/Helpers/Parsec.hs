{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module Yesod.Helpers.Parsec where

import Prelude
import Yesod
import Data.String                          (IsString, fromString)
import Control.Monad                        (void)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative                  ((<*))
#endif

import Text.Parsec
import Text.Parsec.Text                     ()
import Language.Haskell.TH

import Database.Persist.Sql                 (sqlType, PersistFieldSql)
import Text.Parsec.String                   (Parser)
#if MIN_VERSION_base(4,8,0)
#else
import Data.Monoid                          (mconcat)
#endif
import Data.Functor.Identity                (Identity)
import Data.Text                            (Text)
import Data.ByteString                      (ByteString)
import Data.Char                            (isDigit, toUpper)
import Data.Maybe
import Data.Int
import Data.Word
import Network                              (HostName, PortID(..))
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as AT
import qualified Data.Text                  as T
import qualified Text.Parsec.Token          as PT
import qualified Text.Parsec.Number         as PN
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.UTF8       as B8
import qualified Data.ByteString            as B


type GenCharParser u m a = forall s. Stream s m Char => ParsecT s u m a

type CharParser a = GenCharParser () Identity a

-- | a data type that can be encoded into string, and decoded from string.
-- Use this class instead of Show/Read, when you need to control
-- the details of serialization format in DB or config file (like Aeson),
-- and let compiler generate instance of Show/Read automatically.
class SimpleStringRep a where
    simpleEncode :: a -> String
    simpleParser :: CharParser a

instance SimpleStringRep () where
    simpleEncode _ = ""
    simpleParser = return ()

instance SimpleStringRep Double where
    simpleEncode = show
    simpleParser = fmap (either (fromIntegral :: Integer -> Double) id)
                    PN.natFloat

instance SimpleStringRep Int where
    simpleEncode = show
    simpleParser = PN.int

instance SimpleStringRep Int64 where
    simpleEncode = show
    simpleParser = PN.int

instance SimpleStringRep Word32 where
    simpleEncode = show
    simpleParser = PN.nat

instance SimpleStringRep Integer where
    simpleEncode = show
    simpleParser = PN.int

instance SimpleStringRep ByteString where
    simpleEncode = B8.toString . B16.encode

    simpleParser = do
        s <- many1 hexDigit
        let (good, invalid) = B16.decode $ B8.fromString s
        if B.null invalid
            then return good
            else parserFail $ "cannot decode as hex-encoded bytestring"

simpleEncodeParens :: SimpleStringRep a => a -> String
simpleEncodeParens x = mconcat [ "(", simpleEncode x, ")" ]

simpleParseJson :: SimpleStringRep a => String -> A.Value -> AT.Parser a
simpleParseJson name = A.withText name $ \t -> do
    case parse simpleParser "" t of
        Left err -> fail $ "cannot parse as type '" ++ name ++ "': "
                            ++ show err
        Right x -> return x

-- | helper for implement 'simpleParser'
makeSimpleParserByTable :: [(String, a)] -> CharParser a
makeSimpleParserByTable lst =
    choice $
        flip map lst $ \(s, v) ->
            try $ string s >> return v

-- | generate instance somewhat like this
-- a must be an instance of SimpleStringRep
-- instance PersistField a where
--     toPersistValue      = toPersistValue . simpleEncode
--     fromPersistValue    = parsePVByParser simpleParser
-- instance PersistFieldSql a where
--     sqlType _ = SqlString
derivePersistFieldS :: String -> Q [Dec]
derivePersistFieldS s = do
    ss <- [| SqlString |]
    tpv <- [| toPersistValue . simpleEncode |]
    fpv <- [| parsePVByParser simpleParser |]
    return
        [ persistFieldInstanceD (ConT $ mkName s)
            [ FunD 'toPersistValue
                [ Clause [] (NormalB tpv) []
                ]
            , FunD 'fromPersistValue
                [ Clause [] (NormalB fpv) []
                ]
            ]
        , persistFieldSqlInstanceD (ConT $ mkName s)
            [ sqlTypeFunD ss
            ]
        ]

deriveJsonS :: String -> Q [Dec]
deriveJsonS s = do
    to_json <- [| A.String . T.pack . simpleEncode |]
    parse_json <- [| simpleParseJson s  |]
    return
        [ toJsonInstanceD (ConT $ mkName s)
            [ FunD 'toJSON
                [ Clause [] (NormalB to_json) []
                ]
            ]
        , fromJsonInstanceD (ConT $ mkName s)
            [ FunD 'parseJSON
                [ Clause [] (NormalB parse_json) []
                ]
            ]
        ]


-- | try render encoded result of every possible value,
-- if it match the following string, parse successfully.
enumEncodedParser :: (Enum a, Bounded a) =>
    (a -> String)
    -> CharParser a
enumEncodedParser render = choice $ map f [minBound .. maxBound]
    where
        f x = try $ string (render x) >> return x


-- | split a text/string, by a parser
splitByParsec ::
    (Stream s Identity Char, IsString s) =>
    ParsecT s () Identity a
    -> s
    -> Either String [s]
splitByParsec sep t = do
        case parse
            (skipMany sep >> many anyChar `sepEndBy` (eof <|> (void $ many1 sep)))
            "" t
            of
            Left err -> fail $ "failed to split text: " ++ show err
            Right x -> return $ map fromString x


-- | parse time length in the following formats:
-- 1'20" : 1 minute 20 seconds
-- 1′20″ : 1 minute 20 seconds
-- 1′20 : 1 minute 20 seconds
-- 1'20 : 1 minute 20 seconds
-- 00:01:20: 1 minute 20 seconds
-- 01:20: 1 minute 20 seconds
-- 300.05 : 300.05 seconds
parseSeconds :: CharParser Double
parseSeconds = try p_minute_and_sec
                <|> try p_hour_minute_and_sec2
                <|> try p_minute_and_sec2
                <|> p_sec
    where
        p_sec = fmap (either fromIntegral id) naturalOrFloat
        p_minute_and_sec = do
            minute <- natural
            _ <- char '\'' <|> char '′'
            sec <- p_sec
            (void $ char '"' <|> char '″') <|> eof
            return $ fromIntegral minute * 60 + sec

        p_minute_and_sec2 = do
            minute <- natural
            _ <- char ':'
            sec <- p_sec
            (void $ char ':') <|> eof
            return $ fromIntegral minute * 60 + sec

        p_hour_minute_and_sec2 = do
            hour <- natural
            _ <- char ':'
            minute <- natural
            _ <- char ':'
            sec <- p_sec
            (void $ char ':') <|> eof
            return $ fromIntegral hour * 3600 + fromIntegral minute * 60 + sec

-- | remove digit grouping marks, then parse the string as int
parseIntWithGrouping :: Integral a => Char -> CharParser a
parseIntWithGrouping sep = do
    s <- many1 (satisfy allowed_char)
    let s' = filter (/= sep) s
    case parse integer "" s' of
        Left err    -> fail $ show err
        Right x     -> return $ fromIntegral x
    where
        allowed_char c = isDigit c || c == '-' || c == sep


parseByteSizeWithUnit :: Integral a => CharParser a
parseByteSizeWithUnit = do
    sz <- PN.nat
    sz2 <- fmap (fromMaybe 1) $ optionMaybe p_unit
    return $ sz * sz2
    where
        p_unit = do
            uc <- oneOf "kKmMgGtT"
            case toUpper uc of
                'K' -> return 1024
                'M' -> return $ 1024 * 1024
                'G' -> return $ 1024 * 1024 * 1024
                'T' -> return $ 1024 * 1024 * 1024 * 1024
                _   -> fail $ "unknown unit char" ++ [uc]


type ConnectPath = (HostName, PortID)

-- | mainly for config file: parse a string value into
-- a file path or a network host and port
-- example:
-- /path/to/file
--
-- :/path/to/file       -- localhost and unix socket
-- hostname:80          -- hostname and port number
-- hostname:www         -- hostname and service name
parseFileOrConnectPath :: CharParser (Either FilePath ConnectPath)
parseFileOrConnectPath = try (fmap Right parseConnectPath) <|> fmap Left p_file
    where
        p_file = many1 anyChar

{-# DEPRECATED parseFileOrNetworkPath "use parseFileOrConnectPath" #-}
parseFileOrNetworkPath :: CharParser (Either FilePath ConnectPath)
parseFileOrNetworkPath = parseFileOrConnectPath

parseConnectPath :: CharParser ConnectPath
parseConnectPath = do
    hostname <- manyTill hostname_char (char ':')
    if null hostname
        then do
#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
            fmap (("localhost",) . UnixSocket) $ many1 anyChar
#else
            fail $ "UnixSocket is not available on this platform"
#endif
        else do
            fmap (hostname,) parsePortID
    where
        hostname_char = noneOf ":/"

parsePortID :: CharParser PortID
parsePortID = try (fmap (PortNumber . fromIntegral) natural)
                    <|> fmap Service (many1 anyChar)

eol :: CharParser String
eol = try (fmap return newline) <|> (string "\r\n")


-- | This helper function is for encodedListTextareaField.
manySepEndBy :: CharParser b -> CharParser a -> CharParser [a]
manySepEndBy p_sep p = do
    -- if p eats some char of 'p_sep' the following line failed
    skipMany p_sep >> p `sepEndBy` (void $ many1 p_sep)

strictParseSimpleEncoded ::
    (SimpleStringRep a, Stream s Identity Char) =>
    s -> Either ParseError a
strictParseSimpleEncoded t = parse (simpleParser <* eof) "" t

----------------------------------------------------------------------

sqlTypeFunD :: Exp -> Dec
sqlTypeFunD st = FunD 'sqlType
                [ Clause [WildP] (NormalB st) [] ]

persistFieldInstanceD :: Type -> [Dec] -> Dec
persistFieldInstanceD typ =
    InstanceD [] (ConT ''PersistField `AppT` typ)

persistFieldSqlInstanceD :: Type -> [Dec] -> Dec
persistFieldSqlInstanceD typ =
    InstanceD [] (ConT ''PersistFieldSql `AppT` typ)

toJsonInstanceD :: Type -> [Dec] -> Dec
toJsonInstanceD typ =
    InstanceD [] (ConT ''ToJSON `AppT` typ)

fromJsonInstanceD :: Type -> [Dec] -> Dec
fromJsonInstanceD typ =
    InstanceD [] (ConT ''FromJSON `AppT` typ)


parsePVByParser :: Parser a -> PersistValue -> Either Text a
parsePVByParser p pv =
    case fromPersistValue pv of
        Left err    -> Left err
        Right s     ->
            case parse (p <* eof) "" s of
                Left err -> Left $ T.pack $
                                "cannot parsed as persist value: " ++ show err
                Right x -> Right x


-- | like haskellStyle, but with different type signature
simpleLangDef :: Stream s m Char => PT.GenLanguageDef s u m
simpleLangDef = PT.LanguageDef
                    { PT.commentStart   = "{-"
                    , PT.commentEnd     = "-}"
                    , PT.commentLine    = "--"
                    , PT.nestedComments = True
                    , PT.identStart     = letter
                    , PT.identLetter    = alphaNum <|> oneOf "_"
                    , PT.opStart        = PT.opLetter simpleLangDef
                    , PT.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                    , PT.reservedOpNames = []
                    , PT.reservedNames  = []
                    , PT.caseSensitive  = True
                    }
lexer :: Stream s Identity Char => PT.GenTokenParser s u Identity
lexer       = PT.makeTokenParser simpleLangDef

natural :: Stream s Identity Char => ParsecT s u Identity Integer
natural     = PT.natural lexer

float :: Stream s Identity Char => ParsecT s u Identity Double
float       = PT.float lexer

naturalOrFloat :: Stream s Identity Char =>
                    ParsecT s u Identity (Either Integer Double)
naturalOrFloat = PT.naturalOrFloat lexer

integer :: Stream s Identity Char => ParsecT s u Identity Integer
integer       = PT.integer lexer

whiteSpace :: Stream s Identity Char => ParsecT s u Identity ()
whiteSpace  = PT.whiteSpace lexer

lexeme :: Stream s Identity Char =>
    ParsecT s u Identity a -> ParsecT s u Identity a
lexeme      = PT.lexeme lexer

symbol :: Stream s Identity Char => String -> ParsecT s u Identity String
symbol      = PT.symbol lexer

parens :: Stream s Identity Char =>
            ParsecT s u Identity a -> ParsecT s u Identity a
parens      = PT.parens lexer


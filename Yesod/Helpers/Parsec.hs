{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Yesod.Helpers.Parsec where

import Prelude
import Yesod
import Data.String                          (IsString, fromString)
import Control.Monad                        (void)

import Text.Parsec
import Text.Parsec.Text                     ()
import Language.Haskell.TH

import Database.Persist.Sql                 (sqlType, PersistFieldSql)
import Text.Parsec.String                   (Parser)
import Data.Monoid                          (mconcat)
import Data.Functor.Identity                (Identity)
import Data.Text                            (Text)
import Data.ByteString                      (ByteString)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as AT
import qualified Data.Text                  as T
import qualified Text.Parsec.Token          as PT
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

instance SimpleStringRep Double where
    simpleEncode = show
    simpleParser = float

instance SimpleStringRep Int where
    simpleEncode = show
    simpleParser = fmap fromIntegral integer

instance SimpleStringRep Integer where
    simpleEncode = show
    simpleParser = integer

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
            case parse p "" s of
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


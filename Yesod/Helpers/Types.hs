{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Yesod.Helpers.Types where

import Prelude

import Control.Applicative                  ((<$>))
import Data.Time                            (TimeZone, timeZoneOffsetString, parseTime)
import System.Locale                        (defaultTimeLocale)
import Control.Monad                        (mzero, void)
import Data.List                            (intersperse)
import Data.SafeCopy
import Data.Text                            (Text)
import Database.Persist                     (PersistField(..), SqlType(SqlString))
import Database.Persist.Sql                 (PersistFieldSql(..))
import Control.DeepSeq                      (NFData(..))
import Control.DeepSeq.Generics             (genericRnf)
import GHC.Generics                         (Generic)
import qualified System.FilePath.Glob       as G
import Text.Parsec
import Yesod.Helpers.Parsec
import Yesod.Helpers.SafeCopy

-- 用 SafeCopy 提供的实现应该使用空间上更优，但迁移时不知道会不会很麻烦
-- 用 deriveSafeCopySimpleEncoded 因为使用的是字串表达，迁移时比较容易做兼容
-- 只是空间使用多一些。
#define USE_SIMPLE_ENCODED_SAFECOPY

#ifdef USE_SIMPLE_ENCODED_SAFECOPY
-- import Yesod.Helpers.SafeCopy               ( deriveSafeCopySimpleEncoded )
#else
import Data.SafeCopy                        ( deriveSafeCopy, base )
#endif


data Gender = Male | Female
            deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance NFData Gender where rnf = genericRnf

$(derivePersistFieldS "Gender")
$(deriveJsonS "Gender")

#ifdef USE_SIMPLE_ENCODED_SAFECOPY
$(deriveSafeCopySimpleEncoded ''Gender)
#else
$(deriveSafeCopy 0 'base ''Gender)
#endif

instance SimpleStringRep Gender where
    simpleEncode Male   = "male"
    simpleEncode Female = "female"

    simpleParser = choice
        [ try $ string "male" >> return Male
        , try $ string "female" >> return Female
        ]


-- | A URL in Text.
newtype UrlText = UrlText { unUrlText :: Text}
                deriving (Show, Eq, Ord)

instance PersistField UrlText where
    toPersistValue = toPersistValue . unUrlText
    fromPersistValue = fmap UrlText . fromPersistValue

instance PersistFieldSql UrlText where
    sqlType _ = SqlString

instance SafeCopy UrlText where
    getCopy             = contain $ UrlText <$> safeGet
    putCopy (UrlText x) = contain $ safePut x


newtype XTimeZone = XTimeZone { unXTimeZone :: TimeZone }
                    deriving (Show, Read, Eq, Ord)

instance SimpleStringRep XTimeZone where
    simpleEncode = timeZoneOffsetString . unXTimeZone
    simpleParser = do
        manyTill anyChar (eof <|> void space) >>=
            maybe mzero (return . XTimeZone) . (parseTime defaultTimeLocale "%z")

$(derivePersistFieldS "XTimeZone")
$(deriveJsonS "XTimeZone")

instance SafeCopy XTimeZone where
    getCopy = getCopySimpleEncoded
    putCopy = putCopySimpleEncoded


data SimpleVersion = SimpleVersion { unSimpleVersion :: [Int] }
                deriving (Show, Read, Eq, Ord)

$(derivePersistFieldS "SimpleVersion")
$(deriveJsonS "SimpleVersion")

instance SimpleStringRep SimpleVersion where
    simpleEncode (SimpleVersion vs) = concat $ intersperse "." $ map show vs

    simpleParser = fmap (SimpleVersion . map fromIntegral) $ natural `sepBy1` string "."

instance SafeCopy SimpleVersion where
    getCopy = getCopySimpleEncoded
    putCopy = putCopySimpleEncoded


data VerConstraint = VerWithOrder Ordering SimpleVersion
                    | VerWithOrderN Ordering SimpleVersion
                    | VerWithGlob G.Pattern
                    | VerWithGlobN G.Pattern
                    | VerLogicAnd VerConstraint VerConstraint
                    | VerLogicOr VerConstraint VerConstraint
                    deriving (Show, Eq)

$(derivePersistFieldS "VerConstraint")
$(deriveJsonS "VerConstraint")

instance SafeCopy VerConstraint where
    getCopy = getCopySimpleEncoded
    putCopy = putCopySimpleEncoded

instance SimpleStringRep VerConstraint where
    simpleEncode (VerWithOrder GT ver) =
                                concat $ intersperse " " $ [ ">" , simpleEncode ver ]

    simpleEncode (VerWithOrder EQ ver) =
                                concat $ intersperse " " $ [ "==" , simpleEncode ver ]

    simpleEncode (VerWithOrder LT ver) =
                                concat $ intersperse " " $ [ "<" , simpleEncode ver ]

    simpleEncode (VerWithOrderN GT ver) =
                                concat $ intersperse " " $ [ "<=" , simpleEncode ver ]

    simpleEncode (VerWithOrderN EQ ver) =
                                concat $ intersperse " " $ [ "/=" , simpleEncode ver ]

    simpleEncode (VerWithOrderN LT ver) =
                                concat $ intersperse " " $ [ ">=" , simpleEncode ver ]

    simpleEncode (VerWithGlob pat) =
                                concat $ intersperse " " $ [ "==" , G.decompile pat ]

    simpleEncode (VerWithGlobN pat) =
                                concat $ intersperse " " $ [ "/=" , G.decompile pat ]

    simpleEncode (VerLogicAnd c1 c2) =
                                concat $ intersperse " " $
                                    [ "("
                                    , simpleEncode c1
                                    , "&&"
                                    , simpleEncode c2
                                    , ")"
                                    ]

    simpleEncode (VerLogicOr c1 c2) =
                                concat $ intersperse " " $
                                    [ "("
                                    , simpleEncode c1
                                    , "||"
                                    , simpleEncode c2
                                    , ")"
                                    ]

    simpleParser = try (parens p) <|> p
        where
            p = choice
                [ try $ p_ord ">" (VerWithOrder GT)
                , try $ p_ord "==" (VerWithOrder EQ)
                , try $ p_ord "<" (VerWithOrder LT)
                , try $ p_ord ">=" (VerWithOrderN LT)
                , try $ p_ord "<=" (VerWithOrderN GT)
                , try $ p_ord "/=" (VerWithOrderN EQ)
                , try $ p_glob "==" VerWithGlob
                , try $ p_glob "/=" VerWithGlobN
                , try $ p_and
                , try $ p_or
                ]

            p_ord sym ctor = symbol sym >> fmap ctor simpleParser

            p_glob sym ctor = do
                _ <- symbol sym
                s <- manyTill anyChar (eof <|> void space)
                either (const mzero) (return . ctor) $
                        G.tryCompileWith G.compDefault s

            p_and = do
                c1 <- simpleParser
                _ <- symbol "&&"
                c2 <- simpleParser
                return $ VerLogicAnd c1 c2

            p_or = do
                c1 <- simpleParser
                _ <- symbol "||"
                c2 <- simpleParser
                return $ VerLogicOr c1 c2


validateSimpleVersion :: VerConstraint -> SimpleVersion -> Bool
validateSimpleVersion (VerWithOrder GT v0) ver = ver > v0
validateSimpleVersion (VerWithOrder LT v0) ver = ver < v0
validateSimpleVersion (VerWithOrder EQ v0) ver = ver == v0
validateSimpleVersion (VerWithOrderN GT v0) ver = ver <= v0
validateSimpleVersion (VerWithOrderN LT v0) ver = ver >= v0
validateSimpleVersion (VerWithOrderN EQ v0) ver = ver /= v0
validateSimpleVersion (VerWithGlob pat) ver = G.match pat $ simpleEncode ver
validateSimpleVersion (VerWithGlobN pat) ver = not $ G.match pat $ simpleEncode ver
validateSimpleVersion (VerLogicAnd c1 c2) ver =
    validateSimpleVersion c1 ver && validateSimpleVersion c2 ver
validateSimpleVersion (VerLogicOr c1 c2) ver =
    validateSimpleVersion c1 ver || validateSimpleVersion c2 ver

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Yesod.Helpers.Types where

import Prelude                              (Read(..))
import ClassyPrelude.Yesod hiding (Proxy)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LB
import           Data.Proxy                 (Proxy(..))
import qualified Data.Serialize             as SL

import Language.Haskell.TH.Lift             (deriveLift)
import Data.Time                            (TimeZone, timeZoneOffsetString)
#if MIN_VERSION_time(1,5,0)
import Data.Time                            (parseTimeM)
#else
import System.Locale                        (defaultTimeLocale)
import Data.Time                            (parseTime)
#endif

import Data.SafeCopy
import Data.Binary                          (Binary)
import Data.Binary.Orphans                  ()
import Database.Persist.Sql                 (PersistFieldSql(..))
import Control.DeepSeq                      (NFData(..))
import Control.DeepSeq.Generics             (genericRnf)
import qualified System.FilePath.Glob       as G
import qualified Data.Binary                as Binary
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
$(deriveSimpleStringRepEnumBounded "Gender")

#ifdef USE_SIMPLE_ENCODED_SAFECOPY
$(deriveSafeCopySimpleEncoded ''Gender)
#else
$(deriveSafeCopy 0 'base ''Gender)
#endif

instance SimpleEncode Gender where
    simpleEncode Male   = "male"
    simpleEncode Female = "female"



-- | A URL in Text.
newtype UrlText = UrlText { unUrlText :: Text}
  deriving ( Show, Eq, Ord, Typeable, Generic, Binary, NFData
           , Hashable, PersistField, PersistFieldSql
           , FromJSON, ToJSON
           )
$(deriveLift ''UrlText)

instance SafeCopy UrlText where
    getCopy             = contain $ UrlText <$> safeGet
    putCopy (UrlText x) = contain $ safePut x
    errorTypeName _     = "UrlText"


instance RedirectUrl m UrlText where toTextUrl = toTextUrl . unUrlText


newtype XTimeZone = XTimeZone { unXTimeZone :: TimeZone }
                    deriving (Show, Read, Eq, Ord, Typeable, Generic, Binary)

instance SimpleEncode XTimeZone where
    simpleEncode = timeZoneOffsetString . unXTimeZone

instance SimpleStringRep XTimeZone where
    simpleParser = do
        manyTill anyChar (eof Text.Parsec.<|> void space) >>=
            maybe mzero (return . XTimeZone) .
#if MIN_VERSION_time(1,5,0)
                (parseTimeM False defaultTimeLocale "%z")
#else
                (parseTime defaultTimeLocale "%z")
#endif

$(derivePersistFieldS "XTimeZone")
$(deriveJsonS "XTimeZone")

instance NFData XTimeZone where rnf (XTimeZone x) = rnf x

instance SafeCopy XTimeZone where
    getCopy = getCopySimpleEncoded
    putCopy = putCopySimpleEncoded


data SimpleVersion = SimpleVersion { unSimpleVersion :: [Int] }
                deriving (Show, Read, Eq, Ord, Generic)

instance NFData SimpleVersion
$(derivePersistFieldS "SimpleVersion")
$(deriveJsonS "SimpleVersion")

instance SimpleEncode SimpleVersion where
    simpleEncode (SimpleVersion vs) = concat $ intersperse "." $ map show vs

instance SimpleStringRep SimpleVersion where
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
                    deriving (Show, Eq, Generic)

instance NFData VerConstraint where
    rnf (VerWithOrder o sv)     = o `seq` rnf sv
    rnf (VerWithOrderN o sv)    = o `seq` rnf sv
    rnf (VerWithGlob gp)        = seq gp ()
    rnf (VerWithGlobN gp)       = seq gp ()
    rnf (VerLogicAnd x y)       = rnf x `seq` rnf y
    rnf (VerLogicOr x y)        = rnf x `seq` rnf y

$(derivePersistFieldS "VerConstraint")
$(deriveJsonS "VerConstraint")

instance SafeCopy VerConstraint where
    getCopy = getCopySimpleEncoded
    putCopy = putCopySimpleEncoded

instance SimpleEncode VerConstraint where
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

instance SimpleStringRep VerConstraint where
    simpleParser = Text.Parsec.try (parens p) Text.Parsec.<|> p
        where
            p = choice
                [ Text.Parsec.try $ p_ord ">" (VerWithOrder GT)
                , Text.Parsec.try $ p_ord "==" (VerWithOrder EQ)
                , Text.Parsec.try $ p_ord "<" (VerWithOrder LT)
                , Text.Parsec.try $ p_ord ">=" (VerWithOrderN LT)
                , Text.Parsec.try $ p_ord "<=" (VerWithOrderN GT)
                , Text.Parsec.try $ p_ord "/=" (VerWithOrderN EQ)
                , Text.Parsec.try $ p_glob "==" VerWithGlob
                , Text.Parsec.try $ p_glob "/=" VerWithGlobN
                , Text.Parsec.try $ p_and
                , Text.Parsec.try $ p_or
                ]

            p_ord sym ctor = symbol sym >> fmap ctor simpleParser

            p_glob sym ctor = do
                _ <- symbol sym
                s <- manyTill anyChar (eof Text.Parsec.<|> void space)
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


-- | 为把一个 bytestring 放到 yesod url 里
newtype B64UByteStringPathPiece = B64UByteStringPathPiece { unB64UByteStringPathPiece :: ByteString }
                                deriving (Eq, Ord, Show, Read, Typeable, Generic, Binary)

instance PathPiece B64UByteStringPathPiece where
    toPathPiece (B64UByteStringPathPiece bs) =
        fromString $ C8.unpack $ B64U.encode bs

    fromPathPiece t = do
        case B64U.decode (encodeUtf8 t) of
            Left _ -> mzero
            Right bs -> return $ B64UByteStringPathPiece bs

instance FromJSON B64UByteStringPathPiece where
    parseJSON v = do
        fmap fromPathPiece (parseJSON v)
            >>= maybe mzero return

instance ToJSON B64UByteStringPathPiece where
    toJSON = toJSON . toPathPiece


-- | A wrapper type to make some instances using base64-url-encoding
newtype B64UByteString a = B64UByteString { unB64UByteString :: a }
                                deriving (Eq, Ord)

instance Binary a => Show (B64UByteString a) where
  show (B64UByteString bs) = C8.unpack $ B64U.encode $ LB.toStrict $ Binary.encode bs

instance Binary a => Read (B64UByteString a) where
  readsPrec d r =
    catMaybes $ map (merge . first dec) $ readsPrec d r
    where
      merge (Left _, _) = Nothing
      merge (Right x, y) = Just (x, y)

      dec s = do
        bs <- B64U.decode s
        (left_bs, _, y) <- either (\(_, _, x) -> Left x) Right $ Binary.decodeOrFail $ LB.fromStrict bs
        if LB.null left_bs
            then return $ B64UByteString y
            else Left "not all input consumed"

instance Binary a => PathPiece (B64UByteString a) where
  toPathPiece = fromString . show

  fromPathPiece t = either (const Nothing) Just $ do
      bs <- B64U.decode (encodeUtf8 t)
      (left_bs, _, y) <- either (\(_, _, x) -> Left x) Right $ Binary.decodeOrFail $ LB.fromStrict bs
      if LB.null left_bs
          then return $ B64UByteString y
          else Left "not all input consumed"

instance Binary a => FromJSON (B64UByteString a) where
    parseJSON v = do
        fmap fromPathPiece (parseJSON v)
            >>= maybe mzero return

instance Binary a => ToJSON (B64UByteString a) where
    toJSON = toJSON . toPathPiece

instance PersistField a => PersistField (B64UByteString a) where
  toPersistValue   = toPersistValue . unB64UByteString
  fromPersistValue = fmap B64UByteString . fromPersistValue

instance PersistFieldSql a => PersistFieldSql (B64UByteString a) where
  sqlType _ = sqlType (Proxy :: Proxy a)

instance Binary a => Binary (B64UByteString a) where
  put = Binary.put . unB64UByteString
  get = B64UByteString <$> Binary.get


-- | A wrapper type to make it store as bytestring in DB, in JSON format.
newtype JsonSerialized a =  JsonSerialized { unJsonSerialized :: a }
                            deriving (Eq, Ord, Show, Read)

instance (ToJSON a, FromJSON a) => PersistField (JsonSerialized a) where
  toPersistValue   = toPersistValue . LB.toStrict . A.encode . unJsonSerialized
  fromPersistValue t = do
    bs <- fromPersistValue t
    either (Left . fromString) (return . JsonSerialized) $ A.eitherDecode $ LB.fromStrict bs

instance (ToJSON a, FromJSON a) => PersistFieldSql (JsonSerialized a) where
  sqlType _ = sqlType (Proxy :: Proxy ByteString)


-- | CAUTION: Put length (32bit, little-endian) before json encoded strings
instance (ToJSON a, FromJSON a) => SL.Serialize (JsonSerialized a) where
  put (JsonSerialized x) = do SL.putWord32le (fromIntegral $ length lbs)
                              SL.putLazyByteString lbs
                           where lbs = A.encode x

  get = do len <- SL.getWord32le
           lbs <- SL.getLazyByteString (fromIntegral len)
           either fail return $ fmap JsonSerialized $ A.eitherDecode lbs


-- | used in URL, represent a url piece
-- that can be
-- * either a DB key
-- * or a text, which can be uniquely identify the DB record
data KeyOrIdent k i = KI_Key k
                    | KI_Ident i

deriving instance (Show k, Show i) => Show (KeyOrIdent k i)
deriving instance (Read k, Read i) => Read (KeyOrIdent k i)
deriving instance (Eq k, Eq i) => Eq (KeyOrIdent k i)
deriving instance (Ord k, Ord i) => Ord (KeyOrIdent k i)

instance (PathPiece k, PathPiece i) => PathPiece (KeyOrIdent k i) where
    toPathPiece (KI_Key k)      = toPathPiece k
    toPathPiece (KI_Ident i)    = toPathPiece i

    fromPathPiece t =
        asum
            [ fmap KI_Key (fromPathPiece t)
            , fmap KI_Ident (fromPathPiece t)
            ]

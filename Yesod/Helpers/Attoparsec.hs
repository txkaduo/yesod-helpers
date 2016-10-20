module Yesod.Helpers.Attoparsec where

import ClassyPrelude
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Base64     as B64

import Data.Attoparsec.ByteString           (Parser, many1)
import Data.Attoparsec.ByteString.Char8     (satisfy)
import Data.Char                            (isHexDigit, isAlphaNum)
import Numeric                              (readHex)


-- | parse a single hex-encoded byte
hex2 :: Parser Word8
hex2 = do
    h1 <- satisfy isHexDigit
    h0 <- satisfy isHexDigit
    maybe (fail "not a hex number") (return . fst) $ listToMaybe $ readHex [h1, h0]


-- | parse a hex-encoded bytestring
hexEncodedByteString :: Parser ByteString
hexEncodedByteString = fmap B.pack $ many1 hex2


-- | parse a standard base64-encoded bytestring
base64EncodedByteString :: Parser ByteString
base64EncodedByteString = do
    b64str <- many1 $ satisfy is_base64_char
    case B64.decode $ C8.pack b64str of
        Left err -> fail $ "failed to base64-decode: " ++ err
        Right bs -> return bs
    where
        is_base64_char c = isAlphaNum c || c `elem` (asString "+/=")

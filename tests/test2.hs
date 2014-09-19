{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import System.Exit
import Data.Aeson
import Data.Aeson.Types
import Data.Yaml                        (decodeEither)
import Control.Monad.Trans              (lift)
import qualified Data.Text              as T

import Yesod.Helpers.Aeson
import Data.ByteString  (ByteString)

test1 :: FromJSON a => ByteString -> (a -> Parser b) -> IO b
test1 bs f = do
    case decodeEither bs of
        Left err -> do
            putStrLn $ "failed to parse aeson string: " ++ err
            exitFailure
        Right v -> do
            case parseEither f v of
                Left err2 -> do
                    putStrLn $ "failed to parse aeson value: " ++ err2
                    exitFailure
                Right x -> return x

main :: IO ()
main = do
    x <- test1 "{ 'a': 1, 'b': { 'c': 2 }, 'x': '2' }" $ \obj -> runSubFieldParser [] $ do
        -- obj .: "a" >>= (withText "text" $ \t -> return t)
        -- runSubFieldParser $ obj +.: "a" +>>= (withText "text" $ \t -> return t)
        a <- obj +.: "a"
        d <- atField (.:) obj "x" $ withText "text" $ \t -> return t
        c <- atFieldS (.:) obj "x" $ liftWithS withObject "object b" $ \obj2 -> obj2 +.: "d"
        -- c <- reqField obj "b" $ \obj2 -> obj2 +.: "d"
        return ((a::Int) + c + T.length d)
    putStrLn $ "x=" ++ show x

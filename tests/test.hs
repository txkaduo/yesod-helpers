{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude
import System.Exit
import Text.Parsec
import Network                              (PortID(..))

import Yesod.Helpers.Types
import Yesod.Helpers.Parsec

testVerValidate :: VerConstraint -> SimpleVersion -> Bool -> IO ()
testVerValidate c v b = do
    if validateSimpleVersion c v /= b
        then do
            putStrLn $ "constaint '" ++ show c ++ "' validate version '" ++ show v
                        ++ "' does not get expected result: " ++ show b
            exitFailure
        else return ()

tryVerConstraintParse :: String -> IO VerConstraint
tryVerConstraintParse s = do
    case parse simpleParser "" s of
        Left err -> do
            putStrLn $ "failed to parse " ++ show s ++ ": " ++ show err
            exitFailure
        Right x -> return x

testAnyCharParser :: (Eq a, Show a) => CharParser a -> String -> a -> IO ()
testAnyCharParser p s expected = do
    case parse p "" s of
        Left err -> do
            putStrLn $ "failed to parse " ++ show s ++ ": " ++ show err
            exitFailure
        Right x -> do
            if x == expected
                then return ()
                else do
                    putStrLn $ "failed to parse " ++ show s ++ "to expected result"
                    putStrLn $ "expected: " ++ show expected
                    putStrLn $ "actual: " ++ show x
                    exitFailure


testVerConstraint :: IO ()
testVerConstraint = do
    testVerValidate (VerWithOrder GT $ SimpleVersion [0, 9]) (SimpleVersion [1]) True
    testVerValidate (VerWithOrder GT $ SimpleVersion [1]) (SimpleVersion [1, 0]) True
    tryVerConstraintParse "> 0.9"
        >>= flip (flip testVerValidate $ SimpleVersion [1]) True
    tryVerConstraintParse "> 1.0"
        >>= flip (flip testVerValidate $ SimpleVersion [1, 0, 1]) True
    tryVerConstraintParse ">= 1.0"
        >>= flip (flip testVerValidate $ SimpleVersion [1, 0, 1]) True
    tryVerConstraintParse "== 1.*"
        >>= flip (flip testVerValidate $ SimpleVersion [1, 0, 1]) True
    tryVerConstraintParse "/= 1.*"
        >>= flip (flip testVerValidate $ SimpleVersion [1, 0, 1]) False

test_parseFileOrNetworkPath :: IO ()
test_parseFileOrNetworkPath = do
    let f = testAnyCharParser parseFileOrNetworkPath
    f "/path/to/some" $ Left "/path/to/some"
    f ":/path/to/some" $ Right ("localhost", UnixSocket "/path/to/some")
    f "127.0.0.1:80" $ Right ("127.0.0.1", PortNumber (fromIntegral (80::Int)))
    f "127.0.0.1:www" $ Right ("127.0.0.1", Service "www")

test_parseSeconds :: IO ()
test_parseSeconds = do
    let f = testAnyCharParser parseSeconds
    f "10"          10
    f "10.1"        10.1
    f "1'20\""      80
    f "1'20.1\""    80.1
    f "1'20"        80
    f "1′20″"       80
    f "1′20"        80

main :: IO ()
main = do
    testVerConstraint
    test_parseFileOrNetworkPath
    test_parseSeconds

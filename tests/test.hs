{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Main where

import ClassyPrelude
import System.Exit
import Text.Parsec
import Data.Proxy
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Data.ByteString.Base16               as B16
import Data.Time

import Network                              (PortID(..))

import Yesod.Helpers.Types
import Yesod.Helpers.Parsec
import Yesod.Helpers.FuzzyDay
import Yesod.Helpers.Utils

import Data.SafeCopy
import Data.Serialize
import Yesod.Helpers.SafeCopy


testVerValidate :: VerConstraint -> SimpleVersion -> Bool -> IO ()
testVerValidate c v b = do
    if validateSimpleVersion c v /= b
        then do
            putStrLn $ "constaint '" <> tshow c <> "' validate version '" <> tshow v
                        <> "' does not get expected result: " <> tshow b
            exitFailure
        else return ()

tryVerConstraintParse :: String -> IO VerConstraint
tryVerConstraintParse s = do
    case parse simpleParser "" s of
        Left err -> do
            putStrLn $ "failed to parse " <> tshow s <> ": " <> tshow err
            exitFailure
        Right x -> return x

testAnyCharParser :: (Eq a, Show a) => ParsecT String () Identity a -> String -> a -> IO ()
testAnyCharParser p s expected = do
    case parse p "" s of
        Left err -> do
            putStrLn $ "failed to parse " <> tshow s <> ": " <> tshow err
            exitFailure
        Right x -> do
            if x == expected
                then return ()
                else do
                    putStrLn $ "failed to parse " <> tshow s <> "to expected result"
                    putStrLn $ "expected: " <> tshow expected
                    putStrLn $ "actual: " <> tshow x
                    exitFailure


testSimpleStringRepEnumBounded :: forall a. (Eq a, Show a, Enum a, Bounded a, SimpleStringRep a)
                               => Proxy a
                               -> IO ()
testSimpleStringRepEnumBounded _ =
  forM_ [minBound .. maxBound] $ \ (v :: a) -> do
    testAnyCharParser simpleParser (simpleEncode v) v


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
    let f = testAnyCharParser parseFileOrConnectPath
    f "/path/to/some" $ Left "/path/to/some"
    -- f ":/path/to/some" $ Right ("localhost", UnixSocket "/path/to/some")
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
    f "01:20"       80
    f "00:01:20"    80

test_parseIntGrouping :: IO ()
test_parseIntGrouping = do
    let f = testAnyCharParser (parseIntWithGrouping ',')
    f "100,123"     (100123 :: Int)
    f "2,100,123"   (2100123 :: Int)

testAnySafeCopy :: (SafeCopy a, Eq a, Show a) => a -> IO ()
testAnySafeCopy x = do
    let bs = runPut $ safePut x
    putStrLn $ B16.encodeBase16 bs
    putStrLn $ tshow x
    case runGet safeGet bs of
        Left err -> do
                    putStrLn $ "FAIL: safeGet failed: " <> fromString err
                    putStrLn $ "      original value: " <> tshow x
                    exitFailure
        Right x2 -> do
                    if x == x2
                        then
                            putStrLn $ "OK: "  <> tshow x2
                        else do
                            putStrLn $ "FAIL: safeGet return different value: " <> tshow x2
                            putStrLn $ "      original value: " <> tshow x

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Dummy
|]
instance SafeCopy (Key Dummy) where
    putCopy = putCopyAnyId
    getCopy = getCopyAnyId

testSafeCopy :: IO ()
testSafeCopy = do
    testAnySafeCopy (FuzzyDayY 2014)
    testAnySafeCopy (SafeCopyId (toSqlKey 1) :: SafeCopyId Dummy)
    testAnySafeCopy (SafeCopyId (toSqlKey 1134242) :: SafeCopyId Dummy)
    testAnySafeCopy (SafeCopyId (toSqlKey 113424224234) :: SafeCopyId Dummy)


testParseGroups :: IO ()
testParseGroups = do
    test_it (p_ints (char ';')) "1" [1]
    test_it (p_ints (char ';')) "1;2;3;" [1,2,3]
    test_it (p_ints (char ';')) "1;2;3" [1,2,3]

    test_it (p_ints newline) "1\n2\n3\n" [1,2,3]
    test_it (p_ints newline) "1\n2\n3" [1,2,3]

    test_it (p_double newline) "1.0\n2.0\n3.0" [1.0,2.0,3.0]

    where
        p_ints :: ParsecT String () Identity a
               -> ParsecT String () Identity [Int]
        p_ints sep = manySepEndBy sep simpleParser <* eof

        p_double :: ParsecT String () Identity a
                 -> ParsecT String () Identity [Double]
        p_double sep = manySepEndBy sep simpleParser <* eof

        test_it :: (Eq a, Show a) => ParsecT String () Identity a -> String -> a -> IO ()
        test_it p t expected = do
            case parse p "" t of
                Left err -> do
                    putStrLn $
                        "FAIL: testParseGroups failed, parse error: "
                            <> tshow err
                            <> " text was: " <> tshow t
                    exitFailure
                Right xs -> do
                    if xs /= expected
                        then do
                            putStrLn $
                                "FAIL: testParseGroups failed, not expected: "
                                        <> tshow xs
                                        <> ", expect " <> tshow expected
                                        <> ", text was: " <> tshow t
                            exitFailure
                        else return ()

test_humanParseFuzzyDay :: IO ()
test_humanParseFuzzyDay = do
    let f = testAnyCharParser humanParseFuzzyDay
    f "2014.9" $ FuzzyDayYM 2014 9
    f "2014.09" $ FuzzyDayYM 2014 9
    f "2014/1" $ FuzzyDayYM 2014 1
    f "2014 /1" $ FuzzyDayYM 2014 1
    -- f "2014 . 1" $ FuzzyDayYM 2014 1
    f "2014 年 1" $ FuzzyDayYM 2014 1
    f "2014 年 1 月" $ FuzzyDayYM 2014 1
    f "2014 年 1 月 2 日" $ FuzzyDayYMD 2014 1 2

    let f2 = testAnyCharParser humanParseFuzzyDayRange
    f2 "2009.08 - 2012.03" (FuzzyDayYM 2009 8, FuzzyDayYM 2012 3)


humanParseUTCTimeIt :: TimeZone -> String -> [String] -> IO ()
humanParseUTCTimeIt tz std_string strs = do
    utc_t0 <- case humanParseUTCTime tz std_string of
        Nothing -> do
            putStrLn $ "cannot parse time string: " <> fromString std_string
            exitFailure
        Just x -> return x

    forM_ strs $ \s -> do
        case humanParseUTCTime tz s of
            Nothing -> do
                putStrLn $ "cannot parse time string: " <> fromString s
                exitFailure
            Just utc_t -> do
                when (utc_t0 /= utc_t) $ do
                    putStrLn $ "time parsed to: " <> tshow utc_t <>
                                "\ndoes not equal to expected: " <> tshow utc_t0
                    exitFailure

test_humanParseUTCTime :: IO ()
test_humanParseUTCTime = do
    let tz = hoursToTimeZone 8
    humanParseUTCTimeIt tz "2015-01-02 12:53:46+0800"
        [ "2015-01-02 12:53:46"
        , "2015-01-02 13:53:46+0900"
        , "2015年01月02日12时53分46秒"
        ]

main :: IO ()
main = do
    testVerConstraint
    test_parseFileOrNetworkPath
    test_parseSeconds
    test_parseIntGrouping
    testSafeCopy
    testParseGroups
    test_humanParseFuzzyDay
    test_humanParseUTCTime
    testSimpleStringRepEnumBounded (Proxy :: Proxy Gender)

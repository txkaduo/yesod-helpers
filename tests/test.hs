module Main where

import Prelude
import System.Exit
import Text.Parsec

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

main :: IO ()
main = do
    testVerConstraint

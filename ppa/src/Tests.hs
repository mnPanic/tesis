module Tests where

import Test.HUnit (
    Counts,
    Test,
    putTextToHandle,
    putTextToShowS,
    runTestTT,
    test,
    (~:),
    (~?=),
 )

import Test.HUnit.Text (runTestText)

import System.IO (stdout)
import TestCertifier (testCertifier)
import TestND (testND)
import TestParser (testParserLexer)

main :: IO Counts
main = do runTestTT tests

mainT :: IO Counts
mainT = do
    (counts, 0) <- runTestText (putTextToHandle stdout True) tests
    return counts

tests :: Test
tests =
    test
        [ "parser and lexer" ~: testParserLexer
        , "natural deduction" ~: testND
        , "certifier" ~: testCertifier
        ]
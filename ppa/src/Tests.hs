module Tests where

import Test.HUnit (
    Counts,
    Test,
    runTestTT,
    test,
    (~:),
    (~?=),
 )

import TestCertifier (testCertifier)
import TestND (testND)
import TestParser (testParserLexer)

main :: IO Counts
main = do runTestTT tests

tests :: Test
tests =
    test
        [ "parser and lexer" ~: testParserLexer
        , "natural deduction" ~: testND
        , "certifier" ~: testCertifier
        ]
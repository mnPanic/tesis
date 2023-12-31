module ProverTests where

import Lexer (Token (..), lexer)

import Test.HUnit (
    Counts,
    Test,
    runTestTT,
    test,
    (~:),
    (~?=),
 )

main :: IO Counts
main = do runTestTT tests

tests :: Test
tests =
    test
        [ "lexer" ~: testLexer
        -- , "parser" ~: testParser
        ]

testLexer :: Test
testLexer =
    test
        [ lexer "_soMeVarName" ~?= [TokenVar "_soMeVarName"]
        , lexer "SomeVarName" ~?= [TokenVar "SomeVarName"]
        , lexer "Some_CursedVar-Name" ~?= [TokenVar "Some_CursedVar-Name"]
        , lexer "someID" ~?= [TokenId "someID"]
        , lexer "@" ~?= [TokenId "@"]
        , lexer "+" ~?= [TokenId "+"]
        , lexer ">" ~?= [TokenId ">"]
        ]

-- testParser :: Test
-- testParser = test []
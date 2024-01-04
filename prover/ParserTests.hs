module ProverTests where

import Lexer (Token (..), lexer)

import Parser (parseExp)
import Prover (Form (..), Term (..))
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
        , "parser" ~: testParser
        ]

testLexer :: Test
testLexer =
    test
        [ lexer "_soMeVarName" ~?= [TokenVar "_soMeVarName"]
        , lexer "SomeVarName" ~?= [TokenVar "SomeVarName"]
        , lexer "Some_CursedVar-Name" ~?= [TokenVar "Some_CursedVar-Name"]
        , lexer "someID" ~?= [TokenId "someID"]
        , "symbols as ids"
            ~: lexer "+ > @"
            ~?= [TokenId "+", TokenId ">", TokenId "@"]
        , "reserved symbols"
            ~: lexer "~ ! v | ^ & => exists . (forall)"
            ~?= [TokenNot, TokenNot, TokenOr, TokenOr, TokenAnd, TokenAnd, TokenImp, TokenExists, TokenDot, TokenOB, TokenForall, TokenCB]
        ]

parse :: String -> Form
parse = parseExp . lexer

testParser :: Test
testParser =
    test
        [ "ambiguous exists and operator"
            ~: parse "exists Y . exists X . f(X) | g(Y)"
            ~?= FExists
                "Y"
                ( FExists
                    "X"
                    ( FOr
                        (FPred "f" [TVar "X"])
                        (FPred "g" [TVar "Y"])
                    )
                )
        , "complete formula"
            ~: parse
                "exists Y .forall _X .\
                \~num_positivo(_X) => !num_negativo(+(_X, Y)) & q(Y)\
                \| a ^ (false v true)"
            ~?= FExists
                "Y"
                ( FForall
                    "_X"
                    ( FImp
                        (FNot (FPred "num_positivo" [TVar "_X"]))
                        ( FAnd
                            ( FOr
                                ( FAnd
                                    ( FNot
                                        ( FPred "num_negativo" [TFun "+" [TVar "_X", TVar "Y"]]
                                        )
                                    )
                                    (FPred "q" [TVar "Y"])
                                )
                                (FPred "a" [])
                            )
                            (FOr FFalse FTrue)
                        )
                    )
                )
        ]
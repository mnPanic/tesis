module TestParser (testParserLexer) where

import ND.ND (Form (..), Term (..), predVar, propVar, tFun0)
import PPA.Lexer (Alex (Alex), Token (..), TokenClass (..), alexInitUserState, runAlex, runAlex')
import PPA.PPA (Decl (..), Program, ProofStep (PSSuppose, PSThusBy))
import PPA.Parser (parseProgram)

import Test.HUnit (
    Counts,
    Test,
    runTestTT,
    test,
    (~:),
    (~?=),
 )

main :: IO Counts
main = do runTestTT testParserLexer

testParserLexer :: Test
testParserLexer =
    test
        [ "parser" ~: testParser
        -- , "lexer" ~: testLexer
        ]

-- TODO: Hacer andar
-- lexer :: String -> Either String [TokenClass]
-- lexer s = do
--     tokens <- runAlex s (return [])
--     Right $ map (\(Token _ c) -> c) tokens

-- testLexer :: Test
-- testLexer =
--     test
--         [ lexer "_soMeVarName" ~?= Right [TokenVar "_soMeVarName"]
--         , lexer "SomeVarName" ~?= Right [TokenVar "SomeVarName"]
--         , lexer "Some_CursedVar-Name" ~?= Right [TokenVar "Some_CursedVar-Name"]
--         , lexer "someID" ~?= Right [TokenId "someID"]
--         , "symbols as ids"
--             ~: lexer "+ > @"
--             ~?= Right [TokenId "+", TokenId ">", TokenId "@"]
--         , "reserved symbols"
--             ~: lexer "~ ¬ v | ^ & => exists . (forall)"
--             ~?= Right [TokenNot, TokenNot, TokenOr, TokenOr, TokenAnd, TokenAnd, TokenImp, TokenExists, TokenDot, TokenParenOpen, TokenForall, TokenParenClose]
--         ]

testParser :: Test
testParser =
    test
        [ "forms" ~: testParseForms
        , "programs" ~: testParserPrograms
        ]

parseProgram' :: String -> Either String Program
parseProgram' = parseProgram

testParserPrograms :: Test
testParserPrograms =
    test
        [ "program"
            ~: parseProgram'
                "axiom \"some axiom\" : forall X . p(X) & q(X) -> exists Y. r(Y) \
                \theorem \"some thm\" : forall K. p \
                \proof \
                \   suppose \"a\" : a \
                \   thus a by \"a\", \"some axiom\" \
                \end"
            ~?= Right
                [ DAxiom
                    "some axiom"
                    ( FForall "X" $ FImp (FAnd (predVar "p" "X") (predVar "q" "X")) (FExists "Y" (predVar "r" "Y"))
                    )
                , DTheorem
                    "some thm"
                    (FForall "K" (propVar "p"))
                    [ PSSuppose "a" $ propVar "a"
                    , PSThusBy (propVar "a") ["a", "some axiom"]
                    ]
                ]
        ]

testParseForms :: Test
testParseForms =
    test
        [ "ambiguous exists and operator"
            ~: doTestForm
                "exists Y . exists X . f(X) | g(Y)"
                ( FExists
                    "Y"
                    ( FExists
                        "X"
                        ( FOr
                            (FPred "f" [TVar "X"])
                            (FPred "g" [TVar "Y"])
                        )
                    )
                )
        , "parens"
            ~: doTestForm
                "(p(X) -> q(X)) | r(Y)"
                ( FOr
                    ( FImp
                        (FPred "p" [TVar "X"])
                        (FPred "q" [TVar "X"])
                    )
                    (FPred "r" [TVar "Y"])
                )
        , "preds and funcs"
            ~: doTestForm
                "q | p(X, f(Y, K, q), W)"
                ( FOr
                    (FPred "q" [])
                    ( FPred
                        "p"
                        [ TVar "X"
                        , TFun "f" [TVar "Y", TVar "K", TFun "q" []]
                        , TVar "W"
                        ]
                    )
                )
        , "complete formula"
            ~: doTestForm
                "exists Y .forall _X .\
                \~num_positivo(_X) -> num_negativo(+(_X, Y)) & q(Y)\
                \| a & (false | true)"
                ( FExists
                    "Y"
                    ( FForall
                        "_X"
                        ( FImp
                            (FNot (FPred "num_positivo" [TVar "_X"]))
                            ( FAnd
                                ( FOr
                                    ( FAnd
                                        ( FPred "num_negativo" [TFun "+" [TVar "_X", TVar "Y"]]
                                        )
                                        (FPred "q" [TVar "Y"])
                                    )
                                    (FPred "a" [])
                                )
                                (FOr FFalse FTrue)
                            )
                        )
                    )
                )
        , "infix pred" ~: doTestForm "x < y" (FPred "<" [tFun0 "x", tFun0 "y"])
        , "infix pred vars"
            ~: doTestForm
                "forall X . forall Y . X < Y"
                (FForall "X" $ FForall "Y" (FPred "<" [TVar "X", TVar "Y"]))
        , "infix term"
            ~: doTestForm
                "positivo(1 `+` 2)"
                (FPred "positivo" [TFun "+" [tFun0 "1", tFun0 "2"]])
        , "infix pred and terms"
            ~: doTestForm
                "3 < 1 `+` 2"
                (FPred "<" [tFun0 "3", TFun "+" [tFun0 "1", tFun0 "2"]])
        , "infix pred and terms both sides"
            ~: doTestForm
                "3 `+` 5 < 1 `+` 2"
                (FPred "<" [TFun "+" [tFun0 "3", tFun0 "5"], TFun "+" [tFun0 "1", tFun0 "2"]])
        ]

doTestForm :: String -> Form -> Test
doTestForm formStr form = parseProgram' axiomForm ~?= Right [DAxiom "foo" form]
  where
    axiomForm = "axiom \"foo\" : " ++ formStr
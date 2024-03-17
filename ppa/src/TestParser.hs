module TestParser (testParserLexer) where

import Lexer (Alex (Alex), Token (..), TokenClass (..), alexInitUserState, runAlex, runAlex')

import PPA (Decl (..), Program, ProofStep (PSAssume, PSThusBy))

import ND (Form (..), Term (..), predVar, propVar)
import Parser (parseProgram)

import Test.HUnit (
    Counts,
    Test,
    runTestTT,
    test,
    (~:),
    (~?=),
 )

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
parseProgram' s = parseProgram s s

testParserPrograms :: Test
testParserPrograms =
    test
        [ "program"
            ~: parseProgram'
                "axiom \"some axiom\" : forall X . p(X) ^ q(X) => exists Y. r(Y) \
                \theorem \"some thm\" : forall K. p \
                \proof \
                \   assume \"a\" : a; \
                \   thus a by \"a\", \"some axiom\"; \
                \qed"
            ~?= Right
                [ DAxiom
                    "some axiom"
                    ( FForall "X" $ FImp (FAnd (predVar "p" "X") (predVar "q" "X")) (FExists "Y" (predVar "r" "Y"))
                    )
                , DTheorem
                    "some thm"
                    (FForall "K" (propVar "p"))
                    [ PSAssume "a" $ propVar "a"
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
                "(p(X) => q(X)) v r(Y)"
                ( FOr
                    ( FImp
                        (FPred "p" [TVar "X"])
                        (FPred "q" [TVar "X"])
                    )
                    (FPred "r" [TVar "Y"])
                )
        , "preds and funcs"
            ~: doTestForm
                "q v p(X, f(Y, K, q), W)"
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
                \~num_positivo(_X) => num_negativo(+(_X, Y)) & q(Y)\
                \| a ^ (false v true)"
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
        ]

doTestForm :: String -> Form -> Test
doTestForm formStr form = parseProgram' axiomForm ~?= Right [DAxiom "foo" form]
  where
    axiomForm = "axiom \"foo\" : " ++ formStr
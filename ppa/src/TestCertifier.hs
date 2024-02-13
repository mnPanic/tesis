module TestCertifier where

import Certifier (
    findContradiction,
    fromClause,
    fromDNF,
    solve,
    toClause,
 )

import NDProofs (
    EnvItem,
 )

import NDChecker (
    CheckResult (..),
    check,
 )

import ND (
    Env (..),
    Form (..),
    Proof (..),
    Term (..),
    predVar,
    propVar,
 )

import Test.HUnit (
    Counts,
    Test,
    Testable (test),
    assertEqual,
    assertFailure,
    runTestTT,
    (@=?),
    (@?=),
    (~:),
    (~?),
    (~?=),
 )

main :: IO Counts
main = do runTestTT tests

tests :: Test
tests =
    test
        [ "clauses" ~: testClause
        , "findContradiction" ~: testFindContradiction
        , "testSolve" ~: testSolve
        ]

testSolve :: Test
testSolve =
    test
        [ "refutable single clause w/ false"
            ~: doTestSolveEqCheck
                ("h", fromClause [FFalse, propVar "Q"])
                PAndE1
                    { right = propVar "Q"
                    , proofAnd = PAx "h"
                    }
        , "refutable single clause w/ opposites"
            ~: doTestSolveEqCheck
                ("h", fromClause [propVar "A", FNot $ propVar "A"])
                PNotE
                    { form = propVar "A"
                    , proofNotForm =
                        PAndE2
                            { left = propVar "A"
                            , proofAnd = PAx "h"
                            }
                    , proofForm =
                        PAndE1
                            { right = FNot $ propVar "A"
                            , proofAnd = PAx "h"
                            }
                    }
        , "refutable dnf three clauses"
            ~: doTestSolveEqCheck
                ( "h"
                , fromDNF
                    [ [propVar "A", FNot $ propVar "A"]
                    , [FFalse, propVar "Q"]
                    , [FNot $ propVar "B", propVar "B"]
                    ]
                )
                POrE
                    { left =
                        fromDNF
                            [ [propVar "A", FNot $ propVar "A"]
                            , [FFalse, propVar "Q"]
                            ]
                    , right = fromClause [FNot $ propVar "B", propVar "B"]
                    , proofOr = PAx "h"
                    , hypLeft = "h L"
                    , proofAssumingLeft =
                        POrE
                            { left = fromClause [propVar "A", FNot $ propVar "A"]
                            , right = fromClause [FFalse, propVar "Q"]
                            , proofOr = PAx "h L"
                            , hypLeft = "h L L"
                            , proofAssumingLeft =
                                PNotE
                                    { form = propVar "A"
                                    , proofNotForm =
                                        PAndE2
                                            { left = propVar "A"
                                            , proofAnd = PAx "h L L"
                                            }
                                    , proofForm =
                                        PAndE1
                                            { right = FNot $ propVar "A"
                                            , proofAnd = PAx "h L L"
                                            }
                                    }
                            , hypRight = "h L R"
                            , proofAssumingRight =
                                PAndE1
                                    { right = propVar "Q"
                                    , proofAnd = PAx "h L R"
                                    }
                            }
                    , hypRight = "h R"
                    , proofAssumingRight =
                        PNotE
                            { form = propVar "B"
                            , proofNotForm =
                                PAndE1
                                    { right = propVar "B"
                                    , proofAnd = PAx "h R"
                                    }
                            , proofForm =
                                PAndE2
                                    { left = FNot $ propVar "B"
                                    , proofAnd = PAx "h R"
                                    }
                            }
                    }
        , "refutable long"
            ~: doTestSolveCheck
            $ fromDNF
                [
                    [ FExists "x" (predVar "P" "x")
                    , FTrue
                    , propVar "Q"
                    , FNot $ FExists "x" (predVar "P" "x")
                    ]
                , [propVar "X", FFalse, FNot $ propVar "Q"]
                , [propVar "A", propVar "B", FNot $ propVar "A", FNot $ propVar "B"]
                ,
                    [ FForall "y" (predVar "Q" "y")
                    , FTrue
                    , propVar "Q"
                    , FNot $ FForall "y" (predVar "Q" "y")
                    ]
                ]
        , "clause too short not refutable"
            ~: solve ("h", fromDNF [[propVar "X"]])
            ~?= Left "[X] contains no contradicting literals or false"
        , "one clause not refutable"
            ~: solve
                ( "h"
                , fromDNF
                    [
                        [ FExists "x" (predVar "P" "x")
                        , FTrue
                        , propVar "Q"
                        , FNot $ FExists "x" (predVar "P" "x")
                        ]
                    , [propVar "X", FFalse, FNot $ propVar "Q"]
                    , [propVar "A", propVar "B", FNot $ propVar "A", FNot $ propVar "B"]
                    , [propVar "X", FTrue]
                    ]
                )
            ~?= Left "[X,true] contains no contradicting literals or false"
        , "not dnf" ~: solve ("h", FImp FTrue FFalse) ~?= Left "convert to clause: true => false is not a literal"
        ]

doTestSolveEqCheck :: EnvItem -> Proof -> IO ()
doTestSolveEqCheck i@(h, f) expectedProof = do
    let result = solve i
    result @?= Right expectedProof
    let (Right proof) = result
    check (EExtend h f EEmpty) proof FFalse @?= CheckOK

doTestSolveCheck :: Form -> IO ()
doTestSolveCheck f = do
    let result = solve ("h", f)
    case result of
        Right proof -> check (EExtend "h" f EEmpty) proof FFalse @?= CheckOK
        Left err -> assertFailure err

testFindContradiction :: Test
testFindContradiction =
    test
        [ "false contradiction"
            ~: findContradiction
                [ propVar "A"
                , propVar "B"
                , FFalse
                ]
            ~?= Right
                FFalse
        , "no contradiction"
            ~: findContradiction
                [ propVar "A"
                , propVar "B"
                , propVar "C"
                ]
            ~?= Left "[A,B,C] contains no contradicting literals or false"
        , "literals contradicting"
            ~: findContradiction
                [ propVar "A"
                , propVar "B"
                , FNot $ propVar "A"
                , propVar "C"
                ]
            ~?= Right (propVar "A")
        , "more than one literal contradicting returns first"
            ~: findContradiction
                [ FForall "x" (propVar "A")
                , propVar "B"
                , FNot $ propVar "B"
                , propVar "C"
                , FNot $ FForall "x" (propVar "A")
                ]
            ~?= Right (FForall "x" (propVar "A"))
        ]

testClause :: Test
testClause =
    test
        [ "not clause error"
            ~: toClause (FOr (propVar "A") (propVar "B"))
            ~?= Left
                "convert to clause: A v B is not a literal"
        , "not literals error"
            ~: toClause (FNot FTrue)
            ~?= Left "convert to clause: ~true is not a literal"
        , "full clause"
            ~: toClause
                ( FAnd
                    (FNot $ propVar "A")
                    ( FAnd
                        (FExists "x" (predVar "A" "x"))
                        ( FAnd
                            (FForall "x" (predVar "B" "x"))
                            (FAnd FTrue FFalse)
                        )
                    )
                )
            ~?= Right
                [ FNot $ propVar "A"
                , FExists
                    "x"
                    (predVar "A" "x")
                , FForall
                    "x"
                    (predVar "B" "x")
                , FTrue
                , FFalse
                ]
        , "fromClause"
            ~: fromClause [propVar "A", propVar "B", propVar "C", propVar "D"]
            ~?= FAnd
                ( FAnd
                    ( FAnd
                        (propVar "A")
                        (propVar "B")
                    )
                    (propVar "C")
                )
                (propVar "D")
        , "fromDNF"
            ~: fromDNF
                [ [propVar "A", propVar "B", propVar "Q"]
                , [propVar "C", FNot $ propVar "A"]
                , [propVar "X"]
                ]
            ~?= FOr
                ( FOr
                    ( FAnd
                        ( FAnd
                            (propVar "A")
                            (propVar "B")
                        )
                        (propVar "Q")
                    )
                    (FAnd (propVar "C") (FNot $ propVar "A"))
                )
                (propVar "X")
        ]
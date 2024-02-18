module TestCertifier where

import Certifier (
    dnf,
    findContradiction,
    fromClause,
    fromDNF,
    solve,
    toClause,
 )

import NDProofs (
    EnvItem,
    hypForm,
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
    dneg,
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
        , "solve" ~: testSolve
        , "dnf" ~: testDnf
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

testDnf :: Test
testDnf =
    test
        [ "imp elim: x => y / ~x v y"
            ~: doTestDNF
                (FImp x y)
                (FOr (FNot x) y)
        , "dneg elim: ~~x / x" ~: doTestDNF (dneg x) x
        , "not dist over and: ~(x ^ y) / ~x v ~y"
            ~: doTestDNF
                (FNot (FAnd x y))
                (FOr (FNot x) (FNot y))
        , "and assoc: x ^ ((y ^ z) ^ (p ^ q)) / (((x ^ y) ^ z) ^ p) ^ q"
            ~: doTestDNF
                (FAnd x (FAnd (FAnd y z) (FAnd p q)))
                (FAnd (FAnd (FAnd (FAnd x y) z) p) q)
        , "or assoc: x v ((y v z) v (p v q)) / (((x v y) v z) v p) v q"
            ~: doTestDNF
                (FOr x (FOr (FOr y z) (FOr p q)))
                (FOr (FOr (FOr (FOr x y) z) p) q)
        , "not dist over or: ~(x v y) / ~x ^ ~y"
            ~: doTestDNF
                (FNot (FOr x y))
                (FAnd (FNot x) (FNot y))
        , "imp elim + or cong2 + not dist over and: x => ~(y ^ z) / ~x v ~y v ~z"
            ~: doTestDNF
                (FImp x (FNot $ FAnd y z))
                (FOr (FOr (FNot x) (FNot y)) (FNot z))
        , "not cong + imp elim + dnegelim: ~(x => y) / x ^ ~y"
            ~: doTestDNF
                (FNot $ FImp x y)
                (FAnd x (FNot y))
        , "not true y not false"
            ~: doTestDNF
                (FAnd (FNot FTrue) (FNot FFalse))
                (FAnd FFalse FTrue)
        , "and dist over or L"
            ~: doTestDNF
                (FAnd x (FOr y z))
                (FOr (FAnd x y) (FAnd x z))
        , "and dist over or R"
            ~: doTestDNF
                (FAnd (FOr y z) x)
                (FOr (FAnd y x) (FAnd z x))
        , "~ ((x ^ (x => y)) => y)"
            ~: doTestDNF
                (FNot (FImp (FAnd x (FImp x y)) y))
                ( fromDNF
                    [[x, FNot x, FNot y], [x, y, FNot y]]
                )
        ]
  where
    p = propVar "p"
    q = propVar "q"
    x = propVar "x"
    y = propVar "y"
    z = propVar "z"

doTestDNF :: Form -> Form -> IO ()
doTestDNF f fDNF = do
    let hF = hypForm f
    let (fGotDNF, dnfProof) = dnf (hF, f)
    fGotDNF @?= fDNF
    CheckOK @=? check (EExtend hF f EEmpty) dnfProof fDNF
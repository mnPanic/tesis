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
            ~: doTestSolve
                ("h", fromClause [FFalse, propVar "Q"])
                PAndE1
                    { right = propVar "Q"
                    , proofAnd = PAx "h"
                    }
        , "refutable single clause w/ opposites"
            ~: solve ("h", fromClause [propVar "A", FNot $ propVar "A"])
            ~?= Right
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
                    -- , "refutable dnf three clauses"
                    --     ~: solve
                    --         ( "h"
                    --         , fromDNF
                    --             [ [propVar "A", FNot $ propVar "A"]
                    --             , [FFalse, propVar "Q"]
                    --             , [FNot $ propVar "B", propVar "B"]
                    --             ]
                    --         )
                    --     ~?= Right
                    --         POrE
                    --             { left =
                    --                 fromDNF
                    --                     [ [propVar "A", FNot $ propVar "A"]
                    --                     , [FFalse, propVar "Q"]
                    --                     ]
                    --             , right = fromClause [FNot $ propVar "B", propVar "B"]
                    --             , proofOr = PAx "h"
                    --             , hypLeft = "h L"
                    --             , proofAssumingLeft =
                    --                 POrE
                    --                     { left = fromClause [propVar "A", FNot $ propVar "A"]
                    --                     , right = fromClause [FFalse, propVar "Q"]
                    --                     , proofOr = PAx "h L"
                    --                     , hypLeft = "h L L"
                    --                     , proofAssumingLeft =
                    --                         PNotE
                    --                             { form = propVar "A"
                    --                             , proofNotForm =
                    --                                 PAndE2
                    --                                     { left = propVar "A"
                    --                                     , proofAnd = PAx "h L L"
                    --                                     }
                    --                             , proofForm =
                    --                                 PAndE1
                    --                                     { right = FNot $ propVar "A"
                    --                                     , proofAnd = PAx "h L L"
                    --                                     }
                    --                             }
                    --                     , hypRight = "h L R"
                    --                     , proofAssumingRight =
                    --                         PAndE1
                    --                             { right = propVar "Q"
                    --                             , proofAnd = PAx "h L R"
                    --                             }
                    --                     }
                    --             , hypRight = "h R"
                    --             , proofAssumingRight =
                    --                 PNotE
                    --                     { form = propVar "B"
                    --                     , proofNotForm =
                    --                         PAndE2
                    --                             { left = propVar "B"
                    --                             , proofAnd = PAx "h R"
                    --                             }
                    --                     , proofForm =
                    --                         PAndE1
                    --                             { right = FNot $ propVar "B"
                    --                             , proofAnd = PAx "h R"
                    --                             }
                    --                     }
                    --             }
        ]

doTestSolve :: EnvItem -> Proof -> IO ()
doTestSolve i@(h, f) expectedProof = do
    let result = solve i
    result @?= Right expectedProof
    let (Right proof) = result
    check (EExtend h f EEmpty) proof f @?= CheckOK

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
                "A v B is not a literal"
        , "not literals error"
            ~: toClause (FNot FTrue)
            ~?= Left "~true is not a literal"
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
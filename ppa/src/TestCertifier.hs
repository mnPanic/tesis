module TestCertifier where

import Certifier (
    findContradiction,
    fromClause,
    toClause,
 )

import ND (
    Form (..),
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
        ]

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
                (propVar "A")
                ( FAnd
                    (propVar "B")
                    ( FAnd
                        (propVar "C")
                        (propVar "D")
                    )
                )
        ]
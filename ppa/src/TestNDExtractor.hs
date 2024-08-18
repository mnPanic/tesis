module TestNDExtractor (testExtractor) where

import ND (Env (EEmpty), Form (..), Proof (..), Term (..), fPred0, fPred1, tFun1)
import NDChecker (CheckResult (CheckOK), check)
import NDExtractor (translateF, translateP)
import Test.HUnit (
    Assertion,
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
main = do runTestTT testExtractor

testExtractor :: Test
testExtractor =
    test
        [ "translateF" ~: testTranslateForm
        , "translateP" ~: testTranslateProof
        ]

doTestTranslate :: Proof -> Form -> Proof -> Form -> Assertion
doTestTranslate p f expectedP expectedF = do
    assertEqual "original doesn't check" CheckOK (check EEmpty p f)
    assertEqual "expected translated doesn't check" CheckOK (check EEmpty expectedP expectedF)
    let (p', f') = translateP p f r
    expectedF @=? f'
    expectedP @=? p'

assertTranslateChecks :: Proof -> Form -> Assertion
assertTranslateChecks p f = do
    let expectedF' = translateF f r
    let (p', f') = translateP p f r
    expectedF' @=? f'
    assertEqual "original doesn't check" CheckOK (check EEmpty p f)
    assertEqual "translated doesn't check" CheckOK (check EEmpty p' f')

testTranslateProof :: Test
testTranslateProof =
    test
        [ "impE/I + andE1/2" ~: do
            -- (a -> b & a) -> b
            let (a, b) = (fPred0 "a", fPred0 "b")
            let f = FImp (FAnd (FImp a b) a) b
            let p =
                    PImpI
                        { hypAntecedent = "a imp b & a"
                        , proofConsequent =
                            PImpE
                                { antecedent = a
                                , proofImp =
                                    PAndE1
                                        { right = a
                                        , proofAnd = PAx "a imp b & a"
                                        }
                                , proofAntecedent =
                                    PAndE2
                                        { left = FImp a b
                                        , proofAnd = PAx "a imp b & a"
                                        }
                                }
                        }
            -- (~R~R a -> ~R~R b & ~R~R a) -> ~R~R b
            let expectedF = FImp (FAnd (FImp (doubleNegR a) (doubleNegR b)) (doubleNegR a)) (doubleNegR b)
            let expectedP =
                    PImpI
                        { hypAntecedent = "a imp b & a"
                        , proofConsequent =
                            PImpE
                                { antecedent = doubleNegR a
                                , proofImp =
                                    PAndE1
                                        { right = doubleNegR a
                                        , proofAnd = PAx "a imp b & a"
                                        }
                                , proofAntecedent =
                                    PAndE2
                                        { left = FImp (doubleNegR a) (doubleNegR b)
                                        , proofAnd = PAx "a imp b & a"
                                        }
                                }
                        }
            doTestTranslate p f expectedP expectedF
        , "andI" ~: do
            -- a -> a & a
            let a = fPred0 "a"
            let f = FImp a (FAnd a a)
            let p =
                    PImpI
                        { hypAntecedent = "h"
                        , proofConsequent =
                            PAndI
                                { proofLeft = PAx "h"
                                , proofRight = PAx "h"
                                }
                        }
            -- (~R~R a -> ~R~R b & ~R~R a) -> ~R~R b
            let expectedF = FImp (doubleNegR a) (FAnd (doubleNegR a) (doubleNegR a))
            let expectedP = p
            doTestTranslate p f expectedP expectedF
        , "LEM" ~: do
            let a = fPred0 "a"
            assertTranslateChecks PLEM (FOr a (FNot a))
        ]

testTranslateForm :: Test
testTranslateForm =
    test
        [ "simple" ~: do
            let f = FAnd (FNot (fPred0 "a")) (FOr (fPred0 "b") (fPred0 "c"))
            let expected =
                    FAnd
                        (tripleNegR (fPred0 "a"))
                        ( fNotR
                            ( FAnd
                                (tripleNegR (fPred0 "b"))
                                (tripleNegR (fPred0 "c"))
                            )
                        )
            expected ~?= translateF f r
        , "exists" ~: do
            let f = FExists "y" (FAnd FTrue FFalse)
            let expected = fNotR (FForall "y" (fNotR (FAnd FTrue r)))
            expected ~?= translateF f r
        , "complex and complete" ~: do
            let f =
                    FImp
                        ( FAnd
                            (FNot (FPred "p" [TVar "x", tFun1 "f" (TVar "y")]))
                            (FForall "x" (fPred0 "p"))
                        )
                        ( FOr
                            (fPred1 "p" (TVar "x"))
                            (FExists "y" (FAnd FTrue FFalse))
                        )
            let expected =
                    FImp
                        ( FAnd
                            (tripleNegR (FPred "p" [TVar "x", tFun1 "f" (TVar "y")]))
                            (FForall "x" (fNotR $ fNotR (fPred0 "p")))
                        )
                        ( fNotR
                            $ FAnd
                                (tripleNegR (fPred1 "p" (TVar "x")))
                                (fNotR $ fNotR (FForall "y" (fNotR (FAnd FTrue r))))
                        )

            expected ~?= translateF f r
        ]

r :: Form
r = fPred0 "r"

fNotR :: Form -> Form
fNotR f = FImp f r

tripleNegR :: Form -> Form
tripleNegR f = fNotR $ doubleNegR f

doubleNegR :: Form -> Form
doubleNegR f = fNotR $ fNotR f
module TestNDExtractor (testExtractor) where

import ND (Env (EEmpty, EExtend), Form (..), Proof (..), Term (..), fPred0, fPred1, fPredVar, tFun0, tFun1)
import NDChecker (CheckResult (CheckOK), check)
import NDExtractor (dNegRElim, translateF, translateP)
import NDReducer (reduce)
import Test.HUnit (
    Assertion,
    Counts,
    Test,
    Testable (test),
    assertBool,
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
        , "dNegRElim" ~: testDNegRElim
        ]

doTestTranslate :: Proof -> Form -> Proof -> Form -> Assertion
doTestTranslate p f expectedP expectedF = do
    assertEqual "original doesn't check" CheckOK (check EEmpty p f)
    assertEqual "expected translated doesn't check" CheckOK (check EEmpty expectedP expectedF)
    let (p', f') = translateP p f r
    expectedF @=? f'
    expectedP @=? p'

    let reducedP' = reduce p'
    assertEqual "reduced doesn't check" CheckOK (check EEmpty p' f')

assertTranslateChecks :: Proof -> Form -> Assertion
assertTranslateChecks p f = do
    let expectedF' = translateF f r
    let (p', f') = translateP p f r
    expectedF' @=? f'
    assertBool "translated and reduced proofs aren't different" (p /= p')
    assertEqual "original doesn't check" CheckOK (check EEmpty p f)
    assertEqual "translated doesn't check" CheckOK (check EEmpty p' f')
    let reducedP' = reduce p'
    assertEqual "reduced doesn't check" CheckOK (check EEmpty p' f')

testTranslateProof :: Test
testTranslateProof =
    test
        [ "ImpE/I + AndE1/2" ~: do
            -- (a -> b & a) -> b
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
        , "AndI" ~: do
            -- a -> a & a
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
        , "LEM" ~: assertTranslateChecks PLEM (FOr a (FNot a))
        , "OrI1" ~: do
            let f = FImp a (FOr a b)
            let p =
                    PImpI
                        { hypAntecedent = "h"
                        , proofConsequent =
                            POrI1
                                { proofLeft = PAx "h"
                                }
                        }
            assertTranslateChecks p f
        , "OrI2" ~: do
            let f = FImp a (FOr b a)
            let p =
                    PImpI
                        { hypAntecedent = "h"
                        , proofConsequent =
                            POrI2
                                { proofRight = PAx "h"
                                }
                        }
            assertTranslateChecks p f
        , "ForallI + ForallE" ~: do
            let (px, qx) = (fPredVar "p" "x", fPredVar "q" "x")
            let py = fPredVar "p" "y"
            let f = FImp (FForall "x" (FAnd px qx)) (FForall "y" py)
            let p =
                    PImpI
                        { hypAntecedent = "h"
                        , proofConsequent =
                            PForallI
                                { newVar = "x"
                                , proofForm =
                                    PAndE1
                                        { right = qx
                                        , proofAnd =
                                            PForallE
                                                { var = "x"
                                                , form = FAnd px qx
                                                , termReplace = TVar "x"
                                                , proofForall = PAx "h"
                                                }
                                        }
                                }
                        }
            assertTranslateChecks p f
        , "ForallE" ~: do
            let f = FImp (FForall "x" (fPredVar "p" "x")) (fPred1 "p" (tFun0 "f"))
            let p =
                    PImpI
                        { hypAntecedent = "h"
                        , proofConsequent =
                            PForallE
                                { var = "x"
                                , form = fPredVar "p" "x"
                                , termReplace = tFun0 "f"
                                , proofForall = PAx "h"
                                }
                        }
            assertTranslateChecks p f
        ]
  where
    (a, b) = (fPred0 "a", fPred0 "b")

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
                        ( fNotR $
                            FAnd
                                (tripleNegR (fPred1 "p" (TVar "x")))
                                (fNotR $ fNotR (FForall "y" (fNotR (FAnd FTrue r))))
                        )

            expected ~?= translateF f r
        ]

testDNegRElim :: Test
testDNegRElim =
    test
        [ "false" ~: doTestDNegRElim FFalse
        , "true" ~: doTestDNegRElim FTrue
        , "pred" ~: doTestDNegRElim (fPredVar "p" "x")
        , "and" ~: doTestDNegRElim (FAnd (fPred0 "p") (fPred0 "q"))
        , "imp" ~: doTestDNegRElim (FImp (fPred0 "p") (fPred0 "q"))
        ]

doTestDNegRElim :: Form -> Assertion
doTestDNegRElim f = do
    let h = "h"
    let f' = translateF f r
    let p = dNegRElim f h r
    let env = EExtend h (doubleNegR f') EEmpty
    assertEqual "translated doesn't check" CheckOK (check env p f')

r :: Form
r = fPred0 "r"

fNotR :: Form -> Form
fNotR f = FImp f r

tripleNegR :: Form -> Form
tripleNegR f = fNotR $ doubleNegR f

doubleNegR :: Form -> Form
doubleNegR f = fNotR $ fNotR f
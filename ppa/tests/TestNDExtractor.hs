module TestNDExtractor (testExtractor) where

import ND (Env (EEmpty, EExtend), Form (..), Proof (..), Term (..), fPred0, fPred1, fPredVar, tFun0, tFun1)
import NDChecker (CheckResult (CheckOK), check)
import NDExtractor (dNegRElim, rElim, translateF, translateP)
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
        , "rElim" ~: testRElim
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

assertTranslateChecksAllowSame :: Proof -> Form -> Assertion
assertTranslateChecksAllowSame p f = do
    let expectedF' = translateF f r
    let (p', f') = translateP p f r
    expectedF' @=? f'
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
        , "OrE" ~: do
            let f = FImp (FOr a a) a
            let p =
                    PImpI
                        { hypAntecedent = "h Or"
                        , proofConsequent =
                            POrE
                                { left = a
                                , right = a
                                , proofOr = PAx "h Or"
                                , hypLeft = "h a"
                                , proofAssumingLeft = PAx "h a"
                                , hypRight = "h a"
                                , proofAssumingRight = PAx "h a"
                                }
                        }
            assertTranslateChecks p f
        , "OrE + AndE1/2 + ImpE" ~: do
            let f = FImp (FAnd (FOr a b) (FAnd (FImp b c) (FImp a c))) c
            let p =
                    PImpI
                        { hypAntecedent = "h"
                        , proofConsequent =
                            POrE
                                { left = a
                                , right = b
                                , proofOr =
                                    PAndE1
                                        { right = FAnd (FImp b c) (FImp a c)
                                        , proofAnd = PAx "h"
                                        }
                                , hypLeft = "h a"
                                , proofAssumingLeft =
                                    PImpE
                                        { antecedent = a
                                        , proofImp =
                                            PAndE2
                                                { left = FImp b c
                                                , proofAnd =
                                                    PAndE2
                                                        { left = FOr a b
                                                        , proofAnd = PAx "h"
                                                        }
                                                }
                                        , proofAntecedent = PAx "h a"
                                        }
                                , hypRight = "h b"
                                , proofAssumingRight =
                                    PImpE
                                        { antecedent = b
                                        , proofImp =
                                            PAndE1
                                                { right = FImp a c
                                                , proofAnd =
                                                    PAndE2
                                                        { left = FOr a b
                                                        , proofAnd = PAx "h"
                                                        }
                                                }
                                        , proofAntecedent = PAx "h b"
                                        }
                                }
                        }
            assertTranslateChecks p f
        , "NotI + NotE" ~: do
            let f = FImp a (FNot $ FNot a)
            let p =
                    PImpI
                        { hypAntecedent = "h a"
                        , proofConsequent =
                            PNotI
                                { hyp = "h not a"
                                , proofBot =
                                    PNotE
                                        { form = a
                                        , proofNotForm = PAx "h not a"
                                        , proofForm = PAx "h a"
                                        }
                                }
                        }
            assertTranslateChecks p f
        , "true" ~: assertTranslateChecksAllowSame PTrueI FTrue
        , "ExistsI" ~: do
            let f = FImp (fPred1 "p" (tFun0 "k")) (FExists "x" $ fPredVar "p" "x")
            let p =
                    PImpI
                        { hypAntecedent = "h p(k)"
                        , proofConsequent =
                            PExistsI
                                { inst = tFun0 "k"
                                , proofFormWithInst = PAx "h p(k)"
                                }
                        }
            assertTranslateChecks p f
        , "ExistsE" ~: do
            let (px, qx) = (fPredVar "p" "x", fPredVar "q" "x")
            let f = FImp (FExists "x" (FAnd px qx)) (FExists "x" px)
            let p =
                    PImpI
                        { hypAntecedent = "h exists"
                        , proofConsequent =
                            PExistsE
                                { var = "x"
                                , form = FAnd px qx
                                , proofExists = PAx "h exists"
                                , hyp = "h and"
                                , proofAssuming =
                                    PExistsI
                                        { inst = TVar "x"
                                        , proofFormWithInst =
                                            PAndE1
                                                { right = qx
                                                , proofAnd = PAx "h and"
                                                }
                                        }
                                }
                        }
            assertTranslateChecks p f
        , "FalseE" ~: do
            let f' =
                    FImp
                        ( FAnd
                            (FNot (FPred "p" [TVar "x", tFun1 "f" (TVar "y")]))
                            (FForall "x" (fPred0 "p"))
                        )
                        ( FOr
                            (fPred1 "p" (TVar "x"))
                            (FExists "y" (FAnd FTrue FFalse))
                        )

            let f = FImp FFalse f'

            let p =
                    PImpI
                        { hypAntecedent = "h"
                        , proofConsequent = PFalseE (PAx "h")
                        }
            assertTranslateChecks p f
        , "A | true <-> true" ~: do
            let ida = FImp (FOr a FTrue) FTrue
            let vuelta = FImp FTrue (FOr a FTrue)
            let iff = FAnd ida vuelta
            let pIda =
                    PImpI
                        { hypAntecedent = "h a | true"
                        , proofConsequent = PTrueI
                        }

            let pIda' =
                    PImpI
                        { hypAntecedent = "h a | true"
                        , proofConsequent =
                            POrE
                                { left = a
                                , right = FTrue
                                , proofOr = PAx "h a | true"
                                , hypLeft = "h a"
                                , proofAssumingLeft = PTrueI
                                , hypRight = "h true"
                                , proofAssumingRight = PTrueI
                                }
                        }

            let pVuelta =
                    PImpI
                        { hypAntecedent = "h true"
                        , proofConsequent =
                            POrI2
                                { proofRight = PAx "h true"
                                }
                        }

            let pIff =
                    PAndI
                        { proofLeft = pIda'
                        , proofRight = pVuelta
                        }

            assertTranslateChecksAllowSame pIda ida
            assertTranslateChecksAllowSame pIda' ida
            assertTranslateChecksAllowSame pVuelta vuelta
            assertTranslateChecksAllowSame pIff iff
        , "a & a" ~: do
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
            assertTranslateChecksAllowSame p f
        ]
  where
    (a, b, c) = (fPred0 "a", fPred0 "b", fPred0 "c")

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
        , "not" ~: doTestDNegRElim (FNot (fPred0 "p"))
        , "forall" ~: doTestDNegRElim (FForall "x" (fPred0 "p"))
        , "exists" ~: doTestDNegRElim (FExists "x" (fPred0 "p"))
        , "or" ~: doTestDNegRElim (FOr (fPred0 "p") (fPred0 "q"))
        ]

doTestDNegRElim :: Form -> Assertion
doTestDNegRElim f = do
    let h = "h"
    let f' = translateF f r
    let p = dNegRElim f h r
    let env = EExtend h (doubleNegR f') EEmpty
    assertEqual "translated doesn't check" CheckOK (check env p f')

testRElim :: Test
testRElim =
    test
        [ "false" ~: doTestRElim FFalse
        , "true" ~: doTestRElim FTrue
        , "pred" ~: doTestRElim (fPredVar "p" "x")
        , "and" ~: doTestRElim (FAnd (fPred0 "p") (fPred0 "q"))
        , "imp" ~: doTestRElim (FImp (fPred0 "p") (fPred0 "q"))
        , "not" ~: doTestRElim (FNot (fPred0 "p"))
        , "forall" ~: doTestRElim (FForall "x" (fPred0 "p"))
        , "exists" ~: doTestRElim (FExists "x" (fPred0 "p"))
        , "or" ~: doTestRElim (FOr (fPred0 "p") (fPred0 "q"))
        ]

doTestRElim :: Form -> Assertion
doTestRElim f = do
    let h = "h"
    let f' = translateF f r
    let p = rElim f (PAx h) r
    let env = EExtend h r EEmpty
    assertEqual "translated doesn't check" CheckOK (check env p f')

r :: Form
r = fPred0 "r"

fNotR :: Form -> Form
fNotR f = FImp f r

tripleNegR :: Form -> Form
tripleNegR f = fNotR $ doubleNegR f

doubleNegR :: Form -> Form
doubleNegR f = fNotR $ fNotR f
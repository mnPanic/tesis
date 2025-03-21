{-# LANGUAGE QuasiQuotes #-}

module TestNDExtractor (testExtractor) where

import Text.RawString.QQ

import Data.Either (fromRight, isRight)
import Extractor.Extractor (extractWitnessCtx, inlineAxioms)
import Extractor.RProofs (
    dNegRElim,
    rElim,
    rIntro,
    transIntro,
 )
import Extractor.Reducer (reduce)
import Extractor.Translator.Proof (translateF, translateFriedman, translateP)
import Extractor.Types (fromPi02, toPi02)
import ND.Checker (CheckResult (CheckOK), check)
import ND.ND (Env (EEmpty, EExtend), Form (..), HypId, Proof (..), Term (..), fPred0, fPred1, fPredVar, tFun0, tFun1)
import PPA.Certifier (certify, checkContext)
import PPA.PPA (Hypothesis (HAxiom))
import PPA.Parser (parseProgram')
import Result (Result)
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
    (~=?),
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
        , "rIntro" ~: testRIntro
        , "transIntro" ~: testTransIntro
        , "translateFriedman" ~: testTranslateFriedman
        , "extractWitness" ~: testExtractWitness
        ]

doTestTranslate :: Proof -> Form -> Proof -> Form -> Assertion
doTestTranslate p f expectedP expectedF = do
    assertEqual "original doesn't check" CheckOK (check EEmpty p f)
    assertEqual "expected translated doesn't check" CheckOK (check EEmpty expectedP expectedF)
    let (p', f') = translateP p f _r
    expectedF @=? f'
    expectedP @=? p'

    let reducedP' = reduce p'
    assertEqual "reduced doesn't check" CheckOK (check EEmpty p' f')

assertTranslateChecks :: Proof -> Form -> Assertion
assertTranslateChecks p f = do
    let expectedF' = translateF f _r
    let (p', f') = translateP p f _r
    expectedF' @=? f'
    assertBool "translated and reduced proofs aren't different" (p /= p')
    assertEqual "original doesn't check" CheckOK (check EEmpty p f)
    assertEqual "translated doesn't check" CheckOK (check EEmpty p' f')
    let reducedP' = reduce p'
    assertEqual "reduced doesn't check" CheckOK (check EEmpty p' f')

assertTranslateChecksAllowSame :: Proof -> Form -> Assertion
assertTranslateChecksAllowSame p f = do
    let expectedF' = translateF f _r
    let (p', f') = translateP p f _r
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
            expected ~?= translateF f _r
        , "exists" ~: do
            let f = FExists "y" (FAnd FTrue FFalse)
            let expected = fNotR (FForall "y" (fNotR (FAnd FTrue _r)))
            expected ~?= translateF f _r
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
                                (fNotR $ fNotR (FForall "y" (fNotR (FAnd FTrue _r))))
                        )

            expected ~?= translateF f _r
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
    let f' = translateF f _r
    let p = dNegRElim f h _r
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
    let f' = translateF f _r
    let p = rElim f (PAx h) _r
    let env = EExtend h _r EEmpty
    assertEqual "translated doesn't check" CheckOK (check env p f')

testTransIntro :: Test
testTransIntro =
    test
        [ "false" ~: doTestTransIntro FFalse
        , "true" ~: doTestTransIntro FTrue
        , "pred" ~: doTestTransIntro (fPredVar "p" "x")
        , "and" ~: doTestTransIntro (FAnd (fPred0 "p") (fPred0 "q"))
        , "imp" ~: doTestTransIntro (FImp (fPred0 "p") (fPred0 "q"))
        , -- TODO: probar casos de falla del lema rIntro
          "not" ~: doTestTransIntro (FNot (fPred0 "p"))
        , "forall" ~: doTestTransIntro (FForall "x" (fPred0 "p"))
        , "exists" ~: doTestTransIntro (FExists "x" (fPred0 "p"))
        , "or" ~: doTestTransIntro (FOr (fPred0 "p") (fPred0 "q"))
        ]

doTestTransIntro :: Form -> Assertion
doTestTransIntro f = do
    let h = "h"
    let f' = translateF f _r
    let p = transIntro f h _r
    let env = EExtend h f EEmpty
    assertEqual "doesn't check" CheckOK (check env p f')

testRIntro :: Test
testRIntro =
    test
        [ "false" ~: doTestRIntro FFalse
        , "true" ~: doTestRIntro FTrue
        , "pred" ~: doTestRIntro (fPredVar "p" "x")
        , "and" ~: doTestRIntro (FAnd (fPred0 "p") (fPred0 "q"))
        , "and nested"
            ~: let [p, q, r, s] = map fPred0 ["p", "q", "r", "s"]
                in doTestRIntro
                    ( FAnd
                        (FAnd p q)
                        (FAnd q (FAnd r s))
                    )
        ]

doTestRIntro :: Form -> Assertion
doTestRIntro f = do
    let h = "h"
    let f' = fNotR $ translateF f _r
    let p = rIntro f h _r
    let env = EExtend h (fNotR f) EEmpty
    assertEqual "doesn't check" CheckOK (check env p f')

testTranslateFriedman :: Test
testTranslateFriedman =
    test
        [ "exists" ~: do
            let f = FExists "x" (fPred1 "p" (TVar "x"))
            let p =
                    PExistsI
                        { inst = tFun0 "k"
                        , proofFormWithInst = PAx "ax"
                        }
            let pk = fPred1 "p" (tFun0 "k")
            let env = EExtend "ax" (FImp (FImp pk f) f) EEmpty
            f_pi <- assertRight "pi02" $ toPi02 f
            CheckOK @=? check env (fst $ translateFriedman p f_pi) f
        , "forall redundant" ~: do
            let f_exists = FExists "x" (fPred1 "p" (TVar "x"))
            let f = FForall "y" f_exists
            let p =
                    PForallI
                        { newVar = "y"
                        , proofForm =
                            PExistsI
                                { inst = tFun0 "k"
                                , proofFormWithInst = PAx "ax"
                                }
                        }
            let pk = fPred1 "p" (tFun0 "k")
            let env = EExtend "ax" (FImp (FImp pk f_exists) f_exists) EEmpty

            f_pi <- assertRight "pi02" $ toPi02 f
            CheckOK @=? check env (fst $ translateFriedman p f_pi) f
        , "forall" ~: do
            let f_exists = FExists "x" (FPred "p" [TVar "x", TVar "y"])

            let f = FForall "y" f_exists
            let ax = FForall "y" (FPred "p" [tFun0 "k", TVar "y"])
            let p =
                    PForallI
                        { newVar = "y"
                        , proofForm =
                            PExistsI
                                { inst = tFun0 "k"
                                , proofFormWithInst =
                                    PForallE
                                        { var = "y"
                                        , form = FPred "p" [tFun0 "k", TVar "y"]
                                        , termReplace = TVar "y"
                                        , proofForall = PAx "ax"
                                        }
                                }
                        }

            f_pi <- assertRight "pi02" $ toPi02 f
            let (proof, _r) = translateFriedman p f_pi

            -- Si pongo el axioma traducido, va a tener libre a y0 y lo vuela
            let proof' = inlineAxioms [HAxiom "ax" ax] proof _r

            let env = EExtend "ax" ax EEmpty
            CheckOK @=? check env proof' f
        ]

testExtractWitness :: Test
testExtractWitness =
    test
        [ "exists simple"
            ~: assertExtractProgramEquals
                "t"
                []
                (tFun0 "v")
                [r|
                    axiom ax: p(v)
                    theorem t: exists X . p(X)
                    proof
                        take X := v
                        thus p(v) by ax
                    end
                |]
        , "forall"
            ~: assertExtractProgramEquals
                "t"
                [tFun0 "k"]
                (tFun0 "v")
                [r|
                    axiom ax: forall Y . p(v, Y)
                    theorem t: forall Y. exists X . p(X, Y)
                    proof
                        let Y
                        take X := v
                        thus p(v, Y) by ax
                    end
                |]
        , "forall 2"
            ~: assertExtractProgramEquals
                "t"
                [tFun0 "k", tFun0 "r"]
                (tFun0 "v")
                [r|
                    axiom ax: forall X . forall Y . p(v, X, Y)
                    theorem t: forall X. forall Y. exists V . p(V, X, Y)
                    proof
                        let X
                        let Y
                        take V := v
                        thus p(v, X, Y) by ax
                    end   
                |]
        , "forall 2 with rename"
            ~: assertExtractProgramEquals
                "t"
                [tFun0 "k", tFun0 "r"]
                (tFun0 "v")
                [r|
                    axiom ax: forall X . forall Y . p(v, X, Y)
                    theorem t: forall X. forall Y. exists V . p(V, X, Y)
                    proof
                        let X'
                        let Y'
                        take V := v
                        thus p(v, X', Y') by ax
                    end   
                |]
        , "and"
            ~: assertExtractProgramEquals
                "t"
                []
                (tFun0 "v")
                [r|
                    axiom ax_1: p(v)
                    axiom ax_2: q(v)
                    theorem t: exists X . p(X) & q(X)
                    proof
                        take X := v
                        thus p(v) by ax_1
                        thus q(v) by ax_2
                    end
                |]
        ]

assertExtractProgramEquals :: HypId -> [Term] -> Term -> String -> IO ()
assertExtractProgramEquals theorem terms expectedTerm rawProgram = do
    prog <- assertRight "parse program" $ parseProgram' "test" rawProgram
    ctx <- assertRight "certify" $ certify prog
    assertEqual "check failed" (Right ()) (checkContext ctx)

    (ctx_translated, ctx_reduced, obtainedTerm, _) <-
        assertRight "extract failed" $
            extractWitnessCtx ctx theorem terms

    assertEqual "terms don't match" expectedTerm obtainedTerm

    assertEqual "check translated failed" (Right ()) (checkContext ctx_translated)
    assertEqual "check translated+reduced failed" (Right ()) (checkContext ctx_reduced)

assertRight :: String -> Result a -> IO a
assertRight msg res = case res of
    Left err -> assertFailure (msg ++ ": " ++ err)
    Right v -> return v

_r :: Form
_r = fPred0 "__r"

fNotR :: Form -> Form
fNotR f = FImp f _r

tripleNegR :: Form -> Form
tripleNegR f = fNotR $ doubleNegR f

doubleNegR :: Form -> Form
doubleNegR f = fNotR $ fNotR f
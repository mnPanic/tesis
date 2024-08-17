module TestNDExtractor (testExtractor) where

import ND (Form (..), Term (..), fPred0, fPred1, tFun1)
import NDExtractor (translateF)
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
tripleNegR f = fNotR $ fNotR $ fNotR f
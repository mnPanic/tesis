module ProverTests where

import Test.HUnit
import Prover
import Proofs

main :: IO Counts
main = do runTestTT tests

tests :: Test
tests = test [
    "check" ~: testCheck,
    "env" ~: testEnv
    ]

exampleEnv = EExtend "h1" FTrue $ EExtend "h2" FFalse EEmpty

testEnv :: Test
testEnv = test [
    get exampleEnv "h1" ~?= Just FTrue,
    get exampleEnv "h2" ~?= Just FFalse,
    get exampleEnv "h3" ~?= Nothing
    ]

testCheck :: Test
testCheck = test [
    -- PAx
    "A |- A" ~:
        check exampleEnv (PAx "h1") FTrue ~?= CheckOK,
    "A |- B invalid" ~:
        check exampleEnv (PAx "h1") FFalse
            ~?= CheckError exampleEnv (PAx "h1") FFalse "env has hyp h1 for different form",

    -- PImpI
    "A -> A" ~: check EEmpty p1 f1 ~?= CheckOK,
    "A -> (B -> A)" ~: check EEmpty p2 f2 ~?= CheckOK,

    -- Usar la misma etiqueta para diferentes hipótesis
    "A -> (B -> B)" ~: check EEmpty p3 f3 ~?= CheckOK,
    "A -> (B -> A) invalid" ~: check EEmpty p3 f2
        ~?= CheckError (EExtend "x" (FPred "B" []) (EExtend "x" (FPred "A" []) EEmpty)) (PAx "x") (FPred "A" []) "env has hyp x for different form",

    -- PImpE
    "(A -> (B -> C)) -> [(A -> B) -> (A -> C)]" ~:
        check EEmpty p4 f4 ~?= CheckOK,
    "(A -> (B -> C)) -> [(A -> B) -> (A -> C)] err left" ~:
        check EEmpty p4Err1 f4 ~?= CheckError
            (EExtend "h A" (propVar "A") $
             EExtend "h A -> B" (FImp (propVar "A") (propVar "B")) $
             EExtend "h A -> (B -> C)" (FImp (propVar "A") (FImp (propVar "B") (propVar "C")))
             EEmpty)
            (PAx "h B -> C")
            (FImp (propVar "B") (propVar "C"))
            "hyp h B -> C not in env",
    "(A -> (B -> C)) -> [(A -> B) -> (A -> C)] err right" ~:
        check EEmpty p4Err2 f4 ~?=  CheckError
            (EExtend "h A" (propVar "A") $
             EExtend "h A -> B" (FImp (propVar "A") (propVar "B")) $
             EExtend "h A -> (B -> C)" (FImp (propVar "A") (FImp (propVar "B") (propVar "C")))
             EEmpty)
            (PAx "h B")
            (propVar "B")
            "hyp h B not in env",
    -- PFalseE
    "bot -> P" ~: check EEmpty p5 f5 ~?= CheckOK,

    -- PNotE, PNotI
    "P -> ~~P" ~: check EEmpty p6 f6 ~?= CheckOK,
    "~~~P -> ~P" ~: check EEmpty p7 f7 ~?= CheckOK,
    "(A -> B) -> (~B -> ~A)" ~: check EEmpty p8 f8 ~?= CheckOK
    ]
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
    "A |- A" ~:
        check exampleEnv (PAx "h1") FTrue ~?= CheckOK,
    "A |- B invalid" ~:
        check exampleEnv (PAx "h1") FFalse
            ~?= CheckError exampleEnv (PAx "h1") FFalse "env has hyp h1 for different form",
    "A -> A" ~: check EEmpty p1 f1 ~?= CheckOK,
    "A -> (B -> A)" ~: check EEmpty p2 f2 ~?= CheckOK,
    -- Usar la misma etiqueta para diferentes hipótesis
    "A -> (B -> B)" ~: check EEmpty p3 f3 ~?= CheckOK,
    "A -> (B -> A) invalid" ~: check EEmpty p3 f2
        ~?= CheckError (EExtend "x" (FPred "B" []) (EExtend "x" (FPred "A" []) EEmpty)) (PAx "x") (FPred "A" []) "env has hyp x for different form"
    ]
module ProverTests where

import Test.HUnit
    ( (~:), (~?=), runTestTT, Counts, Test, Testable(test) )
import Prover
    ( CheckResult(CheckOK, CheckError),
      Proof(PAx),
      Env(EEmpty, EExtend),
      Form(FImp, FTrue, FFalse, FPred, FAnd, FNot, FOr, FForall, FExists),
      subst,
      get,
      check, Term (TFun, TVar) )
import Proofs
    ( propVar,
      f1,
      p1,
      f2,
      p2,
      f3,
      p3,
      f4,
      p4,
      p4Err1,
      p4Err2,
      f5,
      p5,
      f6,
      p6,
      f7,
      p7,
      f8,
      p8,
      f9,
      p9,
      f10,
      p10, p11, f11, p12LEM, f12, p12, p13, f13, p14, f14, doubleNegElim, p15, f15, p17, f17, p16, f16, f18, p18 )

main :: IO Counts
main = do runTestTT tests

tests :: Test
tests = test [
    "check" ~: testCheck
    , "env" ~: testEnv
    , "subst" ~: testSubst
    ]

exampleEnv :: Env
exampleEnv = EExtend "h1" FTrue $ EExtend "h2" FFalse EEmpty

testEnv :: Test
testEnv = test [
    get exampleEnv "h1" ~?= Just FTrue
    , get exampleEnv "h2" ~?= Just FFalse
    , get exampleEnv "h3" ~?= Nothing
    ]

testTerm :: Term
testTerm = TFun "f" [ TVar "y" ]

testSubst :: Test
testSubst = test [
        subst "x" testTerm FTrue ~?= FTrue
        , subst "x" testTerm FFalse ~?= FFalse
        -- Reemplaza solo los que tienen la misma
        , subst "x" testTerm (FPred "A" [TFun "f" [TVar "x"],
                                         TVar "y",
                                         TVar "x"])
            ~?= FPred "A" [TFun "f" [testTerm],
                           TVar "y",
                           testTerm]
        -- Caso con todos los constructores que no son forall y exists
        , subst "x" testTerm (
            FAnd
                (FNot px)
                (FImp
                    (propVar "A")
                    (FOr (propVar "B") px))
            ) ~?= FAnd
                (FNot pt)
                (FImp
                    (propVar "A")
                    (FOr (propVar "B") pt))
        -- Forall y exists pasan cuando es diferente la var cuantificada
        , subst "x" testTerm (FForall "y" px) ~?= FForall "y" pt
        , subst "x" testTerm (FExists "y" px) ~?= FExists "y" pt
        -- Forall y exists cortan cuando es igual la variable
        , subst "x" testTerm (FForall "x" px) ~?= FForall "x" px
        , subst "x" testTerm (FExists "x" px) ~?= FExists "x" px
    ]
    where px = FPred "P" [TVar "x"]
          pt = FPred "P" [testTerm]

testCheck :: Test
testCheck = test [
    -- PAx
    "A |- A" ~: check exampleEnv (PAx "h1") FTrue ~?= CheckOK
    , "A |- B invalid" ~:
        check exampleEnv (PAx "h1") FFalse
            ~?= CheckError exampleEnv (PAx "h1") FFalse "env has hyp h1 for different form"

    -- PImpI
    , "A -> A" ~: check EEmpty p1 f1 ~?= CheckOK
    , "A -> (B -> A)" ~: check EEmpty p2 f2 ~?= CheckOK

    -- Usar la misma etiqueta para diferentes hipótesis
    , "A -> (B -> B)" ~: check EEmpty p3 f3 ~?= CheckOK
    , "A -> (B -> A) invalid" ~: check EEmpty p3 f2
        ~?= CheckError (EExtend "x" (FPred "B" []) (EExtend "x" (FPred "A" []) EEmpty)) (PAx "x") (FPred "A" []) "env has hyp x for different form"

    -- PImpE
    , "(A -> (B -> C)) -> [(A -> B) -> (A -> C)]" ~:
        check EEmpty p4 f4 ~?= CheckOK
    , "(A -> (B -> C)) -> [(A -> B) -> (A -> C)] err left" ~:
        check EEmpty p4Err1 f4 ~?= CheckError
            (EExtend "h A" (propVar "A") $
             EExtend "h A -> B" (FImp (propVar "A") (propVar "B")) $
             EExtend "h A -> (B -> C)" (FImp (propVar "A") (FImp (propVar "B") (propVar "C")))
             EEmpty)
            (PAx "h B -> C")
            (FImp (propVar "B") (propVar "C"))
            "hyp h B -> C not in env"
    , "(A -> (B -> C)) -> [(A -> B) -> (A -> C)] err right" ~:
        check EEmpty p4Err2 f4 ~?=  CheckError
            (EExtend "h A" (propVar "A") $
             EExtend "h A -> B" (FImp (propVar "A") (propVar "B")) $
             EExtend "h A -> (B -> C)" (FImp (propVar "A") (FImp (propVar "B") (propVar "C")))
             EEmpty)
            (PAx "h B")
            (propVar "B")
            "hyp h B not in env"
    -- PFalseE
    , "bot -> P" ~: check EEmpty p5 f5 ~?= CheckOK

    -- PNotE, PNotI
    , "P -> ~~P" ~: check EEmpty p6 f6 ~?= CheckOK
    , "~~~P -> ~P" ~: check EEmpty p7 f7 ~?= CheckOK
    , "(A -> B) -> (~B -> ~A)" ~: check EEmpty p8 f8 ~?= CheckOK

    -- And y OR
    , "(~A v ~B) -> ~(A ^ B)" ~: check EEmpty p10 f10 ~?= CheckOK
    , "((A ^ B) -> C) <-> (A -> (B -> C))" ~: check EEmpty p11 f11 ~?= CheckOK
    , "~~(A v ~A) con LEM" ~: check EEmpty p12LEM f12 ~?= CheckOK
    , "~~(A v ~A) sin LEM" ~: check EEmpty p12 f12 ~?= CheckOK

    -- equivalencias
    , "(A ^ true) <-> A"  ~: check EEmpty p13 f13 ~?= CheckOK
    , "(A v true) <-> true" ~: check EEmpty p14 f14 ~?= CheckOK

    -- implicaciones de LK
    , "~~P -> P" ~: check EEmpty p9 f9 ~?= CheckOK
    , "~~P -> P con macro" ~: check EEmpty (doubleNegElim $ propVar "A") f9 ~?= CheckOK
    , "~(A ^ B) -> (~A v ~B)" ~: check EEmpty p15 f15 ~?= CheckOK
    , "~A ^ ~B -> ~(A v B)" ~: check EEmpty p17 f17 ~?= CheckOK
    , "~(A v B) -> ~A ^ ~B" ~: check EEmpty p16 f16 ~?= CheckOK
    , "Good(y) -> Exists x. Good(x)" ~: check EEmpty p18 f18 ~?= CheckOK
    ]
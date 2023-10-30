module ProverTests where

import Test.HUnit
    ( (~:), (~?=), runTestTT, Counts, Test, Testable(test) )
import Prover
    ( CheckResult(CheckOK, CheckError),
      Proof(..),
      Env(EEmpty, EExtend),
      Form(FImp, FTrue, FFalse, FPred, FAnd, FNot, FOr, FForall, FExists),
      subst,
      get,
      check, fv, Term (TFun, TVar), fvE )

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
      p10, p11, f11, p12LEM, f12, p12, p13, f13, p14, f14, doubleNegElim, p15, f15, p17, f17, p16, f16, f18, p18, p21, f21, f20, p20, p22, f22, p20', f20', p19, f19, p23Ida, f23Ida, p24Ida, f24Ida, p23Vuelta, f23Vuelta )

import qualified Data.Set as Set

main :: IO Counts
main = do runTestTT tests

tests :: Test
tests = test [
    "check" ~: testCheck
    , "env" ~: testEnv
    , "subst" ~: testSubst
    , "fv" ~: testFV
    ]

exampleEnv :: Env
exampleEnv = EExtend "h1" FTrue $ EExtend "h2" FFalse EEmpty

testEnv :: Test
testEnv = test [
    get exampleEnv "h1" ~?= Just FTrue
    , get exampleEnv "h2" ~?= Just FFalse
    , get exampleEnv "h3" ~?= Nothing
    ]

longForm :: Form
longForm = FExists "z" $ FForall "x" (
        FImp
            (FPred "A" [TVar "y"] )
            (FNot $ FAnd
                (FOr FTrue (FPred "B" [TVar "y"]))
                (FPred "A" [ TVar "x", TVar "y", TFun "f" [TVar "z" ]]))
    )

testFV :: Test
testFV = test [
        fv longForm ~?= Set.singleton "y"
        , fvE (EExtend "h1" (FPred "A" [TVar "w"]) $ EExtend "h2" longForm $ EEmpty) ~?= Set.fromList ["w", "y"]
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

    -- Exists y forall
    , "Good(y) => Exists x. Good(x)" ~: check EEmpty p18 f18 ~?= CheckOK
    , "Exists x. A(x) ^ B(x) => Exists y. A(y)" ~: check EEmpty p19 f19 ~?= CheckOK
    , "Forall x. A(x) ^ B(x) => Forall x. A(x)" ~: check EEmpty p20 f20 ~?= CheckOK
    , "Forall x. A(x) ^ B(x) => Forall y. A(y)" ~: check EEmpty p20' f20' ~?= CheckOK
    , "Forall x. A(x) => Exists x. B(x)" ~: check EEmpty p22 f22 ~?=
        CheckError
            (EExtend "h Forall x. A(x)" (FForall "x" (FPred "A" [TVar "x"])) EEmpty)
            (PForallE "x" (FPred "A" [TVar "x"]) (PAx "h Forall x. A(x)") (TVar "x"))
            (FPred "B" [TVar "x"])
            "form FPred \"B\" [TVar \"x\"] /= (FPred \"A\" [TVar \"x\"]){x := TVar \"x\"}"
    , "A(x) => Forall x. A(x)" ~: check EEmpty p21 f21 ~?=
        CheckError
            (EExtend "h A(x)" (FPred "A" [TVar "x"]) EEmpty)
            (PForallI (PAx "h A(x)"))
            (FForall "x" (FPred "A" [TVar "x"]))
            "env shouldn't contain fv 'x'"

    -- DeMorgan de Exists y Forall
    , "V x. A(x) => ~ E x. ~A(x)" ~: check EEmpty p23Ida f23Ida ~?= CheckOK
    , "~E x. ~A(x) => V x. A(x)" ~: check EEmpty p23Vuelta f23Vuelta ~?= CheckOK
    , "E x. A(x) => ~ V x. ~A(x)" ~: check EEmpty p24Ida f24Ida ~?= CheckOK
    ]
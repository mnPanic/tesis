module ProverTests where

import Prover (
    CheckResult (CheckError, CheckOK),
    Env (EEmpty, EExtend),
    Form (FAnd, FExists, FFalse, FForall, FImp, FNot, FOr, FPred, FTrue),
    Proof (..),
    Term (TFun, TVar),
    check,
    fv,
    fvE,
    get,
    subst,
 )
import Test.HUnit (
    Counts,
    Test,
    Testable (test),
    runTestTT,
    (~:),
    (~?),
    (~?=),
 )

import Proofs (
    doubleNegElim,
    f1,
    f10,
    f11,
    f12,
    f13,
    f14,
    f15,
    f16,
    f17,
    f18,
    f19,
    f2,
    f20,
    f20',
    f21,
    f22,
    f23Ida,
    f23Vuelta,
    f24Ida,
    f24Vuelta,
    f3,
    f4,
    f5,
    f6,
    f7,
    f8,
    f9,
    p1,
    p10,
    p11,
    p12,
    p12LEM,
    p13,
    p14,
    p15,
    p16,
    p17,
    p18,
    p19,
    p2,
    p20,
    p20',
    p21,
    p22,
    p23Ida,
    p23Vuelta,
    p24Ida,
    p24Vuelta,
    p3,
    p4,
    p4Err1,
    p4Err2,
    p5,
    p6,
    p7,
    p8,
    p9,
    predVar,
    propVar,
 )

import Data.Set qualified as Set

main :: IO Counts
main = do runTestTT tests

tests :: Test
tests =
    test
        [ "check" ~: testCheck
        , "env" ~: testEnv
        , "subst" ~: testSubst
        , "fv" ~: testFV
        , "alphaEq" ~: testAlphaEq
        ]

exampleEnv :: Env
exampleEnv = EExtend "h1" FTrue $ EExtend "h2" FFalse EEmpty

testEnv :: Test
testEnv =
    test
        [ get exampleEnv "h1" ~?= Just FTrue
        , get exampleEnv "h2" ~?= Just FFalse
        , get exampleEnv "h3" ~?= Nothing
        ]

longForm :: Form
longForm =
    FExists "z"
        $ FForall
            "x"
            ( FImp
                (FPred "A" [TVar "y"])
                ( FNot
                    $ FAnd
                        (FOr FTrue (FPred "B" [TVar "y"]))
                        (FPred "A" [TVar "x", TVar "y", TFun "f" [TVar "z"]])
                )
            )

testFV :: Test
testFV =
    test
        [ fv longForm ~?= Set.singleton "y"
        , fvE (EExtend "h1" (FPred "A" [TVar "w"]) $ EExtend "h2" longForm $ EEmpty) ~?= Set.fromList ["w", "y"]
        ]

testTerm :: Term
testTerm = TFun "f" [TVar "y"]

testAlphaEq :: Test
testAlphaEq =
    test
        [ "terms"
            ~: test
                [ "equal vars no subst"
                    ~: test
                        [ "eq" ~: TVar "x" == TVar "x" ~?= True
                        , "neq" ~: TVar "y" == TVar "x" ~?= False
                        ]
                , "fun name eq" ~: TFun "f1" [] == TFun "f1" [] ~?= True
                , "fun name neq" ~: TFun "f1" [] == TFun "f2" [] ~?= False
                , "fun arity"
                    ~: TFun "f" [TVar "x", TVar "y"]
                    == TFun "f" [TVar "x"]
                    ~?= False
                , "fun vars neq"
                    ~: TFun "f" [TVar "x", TVar "y"]
                    == TFun "f" [TVar "x", TVar "z"]
                    ~?= False
                , "fun vars eq"
                    ~: TFun "f" [TVar "x", TVar "y"]
                    == TFun "f" [TVar "x", TVar "y"]
                    ~?= True
                ]
        , "true" ~: FTrue == FTrue ~?= True
        , "predicate name eq " ~: FPred "A" [] == FPred "A" [] ~?= True
        , "predicate name neq " ~: FPred "A" [] == FPred "B" [] ~?= False
        , "predicate arity" ~: FPred "A" [] == FPred "A" [TVar "x"] ~?= False
        , "forall eq"
            ~: FForall "x" (FPred "A" [TVar "x"])
            == FForall "y" (FPred "A" [TVar "y"])
            ~?= True
        , "exists eq"
            ~: FExists "x" (FPred "A" [TVar "x"])
            == FExists "y" (FPred "A" [TVar "y"])
            ~?= True
        , "neq free var"
            ~: FExists "x" (FPred "A" [TVar "z"])
            == FExists "y" (FPred "A" [TVar "x"])
            ~?= False
        , "swapped vars (need two substs)"
            ~: FExists "x" (FForall "y" (FPred "A" [TVar "x", TVar "y"]))
            == FExists "y" (FForall "x" (FPred "A" [TVar "y", TVar "x"]))
            ~?= True
        , "exists and"
            ~: FExists "x" (FAnd (FPred "A" [TVar "x"]) (FPred "A" [TVar "y"]))
            == FExists "x" (FAnd (FPred "A" [TVar "x"]) (FPred "A" [TVar "x"]))
            ~?= False
        , "and exists"
            ~: FAnd
                (FExists "x" (FPred "A" [TVar "x"]))
                (FExists "x" (FPred "A" [TVar "y"]))
            == FAnd
                (FExists "x" (FPred "A" [TVar "x"]))
                (FExists "x" (FPred "A" [TVar "x"]))
            ~?= False
        ]

testSubst :: Test
testSubst =
    test
        [ subst "x" testTerm FTrue ~?= FTrue
        , subst "x" testTerm FFalse ~?= FFalse
        , -- Reemplaza solo los que tienen la misma
          subst
            "x"
            testTerm
            ( FPred
                "A"
                [ TFun "f" [TVar "x"]
                , TVar "y"
                , TVar "x"
                ]
            )
            ~?= FPred
                "A"
                [ TFun "f" [testTerm]
                , TVar "y"
                , testTerm
                ]
        , -- Caso con todos los constructores que no son forall y exists
          subst
            "x"
            testTerm
            ( FAnd
                (FNot px)
                ( FImp
                    (propVar "A")
                    (FOr (propVar "B") px)
                )
            )
            ~?= FAnd
                (FNot pt)
                ( FImp
                    (propVar "A")
                    (FOr (propVar "B") pt)
                )
        , -- Forall y exists pasan cuando es diferente la var cuantificada (sin captura)
          subst "x" testTerm (FForall "w" px) ~?= FForall "w" pt
        , subst "x" testTerm (FExists "w" px) ~?= FExists "w" pt
        , -- Forall y exists cortan cuando es igual la variable
          subst "x" testTerm (FForall "x" px) ~?= FForall "x" px
        , subst "x" testTerm (FExists "x" px) ~?= FExists "x" px
        , -- Evita captura de variables (alpha renombra)
          subst
            "x"
            (TFun "f" [TVar "z", TVar "y"])
            (FForall "z" (FPred "P" [TVar "x", TVar "z"]))
            ~?= FForall
                "z0"
                (FPred "P" [TFun "f" [TVar "z", TVar "y"], TVar "z0"])
        , subst
            "x"
            (TFun "f" [TVar "z", TVar "y"])
            (FExists "z" (FPred "P" [TVar "x", TVar "z"]))
            ~?= FExists
                "z0"
                (FPred "P" [TFun "f" [TVar "z", TVar "y"], TVar "z0"])
        , subst
            "z"
            (TFun "f" [TVar "x", TVar "y"])
            ( FForall
                "x"
                ( FAnd
                    (FPred "P" [TVar "x"])
                    (FForall "y" (FPred "Q" [TVar "y"]))
                )
            )
            ~?= FForall
                "x0"
                ( FAnd
                    (FPred "P" [TVar "x0"])
                    (FForall "y0" (FPred "Q" [TVar "y0"]))
                )
        , -- Misma var anidada
          subst
            "z"
            (TFun "f" [TVar "x", TVar "y"])
            ( FForall
                "x"
                ( FAnd
                    (FPred "P" [TVar "x"])
                    (FForall "x" (FPred "Q" [TVar "x"]))
                )
            )
            ~?= FForall
                "x0"
                ( FAnd
                    (FPred "P" [TVar "x0"])
                    (FForall "x0" (FPred "Q" [TVar "x0"]))
                )
        ]
  where
    px = FPred "P" [TVar "x"]
    pt = FPred "P" [testTerm]

testCheck :: Test
testCheck =
    test
        [ -- PAx
          "A |- A" ~: check exampleEnv (PAx "h1") FTrue ~?= CheckOK
        , "A |- B invalid"
            ~: check exampleEnv (PAx "h1") FFalse
            ~?= CheckError exampleEnv (PAx "h1") FFalse "env has hyp h1 for different form"
        , -- PImpI
          "A -> A" ~: check EEmpty p1 f1 ~?= CheckOK
        , "A -> (B -> A)" ~: check EEmpty p2 f2 ~?= CheckOK
        , -- Usar la misma etiqueta para diferentes hipótesis
          "A -> (B -> B)" ~: check EEmpty p3 f3 ~?= CheckOK
        , "A -> (B -> A) invalid"
            ~: check EEmpty p3 f2
            ~?= CheckError
                (EExtend "x" (FPred "B" []) (EExtend "x" (FPred "A" []) EEmpty))
                (PAx "x")
                (FPred "A" [])
                "env has hyp x for different form"
        , -- PImpE
          "(A -> (B -> C)) -> [(A -> B) -> (A -> C)]"
            ~: check EEmpty p4 f4
            ~?= CheckOK
        , "(A -> (B -> C)) -> [(A -> B) -> (A -> C)] err left"
            ~: check EEmpty p4Err1 f4
            ~?= CheckError
                ( EExtend "h A" (propVar "A")
                    $ EExtend "h A -> B" (FImp (propVar "A") (propVar "B"))
                    $ EExtend
                        "h A -> (B -> C)"
                        (FImp (propVar "A") (FImp (propVar "B") (propVar "C")))
                        EEmpty
                )
                (PAx "h B -> C")
                (FImp (propVar "B") (propVar "C"))
                "hyp h B -> C not in env"
        , "(A -> (B -> C)) -> [(A -> B) -> (A -> C)] err right"
            ~: check EEmpty p4Err2 f4
            ~?= CheckError
                ( EExtend "h A" (propVar "A")
                    $ EExtend "h A -> B" (FImp (propVar "A") (propVar "B"))
                    $ EExtend
                        "h A -> (B -> C)"
                        (FImp (propVar "A") (FImp (propVar "B") (propVar "C")))
                        EEmpty
                )
                (PAx "h B")
                (propVar "B")
                "hyp h B not in env"
        , -- PFalseE
          "bot -> P" ~: check EEmpty p5 f5 ~?= CheckOK
        , -- PNotE, PNotI
          "P -> ~~P" ~: check EEmpty p6 f6 ~?= CheckOK
        , "~~~P -> ~P" ~: check EEmpty p7 f7 ~?= CheckOK
        , "(A -> B) -> (~B -> ~A)" ~: check EEmpty p8 f8 ~?= CheckOK
        , -- And y OR
          "(~A v ~B) -> ~(A ^ B)" ~: check EEmpty p10 f10 ~?= CheckOK
        , "((A ^ B) -> C) <-> (A -> (B -> C))" ~: check EEmpty p11 f11 ~?= CheckOK
        , "~~(A v ~A) con LEM" ~: check EEmpty p12LEM f12 ~?= CheckOK
        , "~~(A v ~A) sin LEM" ~: check EEmpty p12 f12 ~?= CheckOK
        , -- equivalencias
          "(A ^ true) <-> A" ~: check EEmpty p13 f13 ~?= CheckOK
        , "(A v true) <-> true" ~: check EEmpty p14 f14 ~?= CheckOK
        , -- implicaciones de LK
          "~~P -> P" ~: check EEmpty p9 f9 ~?= CheckOK
        , "~~P -> P con macro" ~: check EEmpty (doubleNegElim $ propVar "A") f9 ~?= CheckOK
        , "~(A ^ B) -> (~A v ~B)" ~: check EEmpty p15 f15 ~?= CheckOK
        , "~A ^ ~B -> ~(A v B)" ~: check EEmpty p17 f17 ~?= CheckOK
        , "~(A v B) -> ~A ^ ~B" ~: check EEmpty p16 f16 ~?= CheckOK
        , -- Exists y forall
          "Good(y) => Exists x. Good(x)" ~: check EEmpty p18 f18 ~?= CheckOK
        , "Exists x. A(x) ^ B(x) => Exists y. A(y)" ~: check EEmpty p19 f19 ~?= CheckOK
        , "Forall x. A(x) ^ B(x) => Forall x. A(x)" ~: check EEmpty p20 f20 ~?= CheckOK
        , "Forall x. A(x) ^ B(x) => Forall y. A(y)" ~: check EEmpty p20' f20' ~?= CheckOK
        , "Forall x. A(x) => Exists x. B(x)"
            ~: check EEmpty p22 f22
            ~?= CheckError
                (EExtend "h Forall x. A(x)" (FForall "x" (FPred "A" [TVar "x"])) EEmpty)
                (PForallE "x" (FPred "A" [TVar "x"]) (PAx "h Forall x. A(x)") (TVar "x"))
                (FPred "B" [TVar "x"])
                "form FPred \"B\" [TVar \"x\"] /= (FPred \"A\" [TVar \"x\"]){x := TVar \"x\"}"
        , "A(x) => Forall x. A(x)"
            ~: check EEmpty p21 f21
            ~?= CheckError
                (EExtend "h A(x)" (FPred "A" [TVar "x"]) EEmpty)
                (PForallI (PAx "h A(x)"))
                (FForall "x" (FPred "A" [TVar "x"]))
                "env shouldn't contain fv 'x'"
        , -- DeMorgan de Exists y Forall
          "V x. A(x) => ~E x. ~A(x)" ~: check EEmpty p23Ida f23Ida ~?= CheckOK
        , "~E x. ~A(x) => V x. A(x)" ~: check EEmpty p23Vuelta f23Vuelta ~?= CheckOK
        , "E x. A(x) => ~V x. ~A(x)" ~: check EEmpty p24Ida f24Ida ~?= CheckOK
        , "~V x. ~A(x) => E x. A(x)" ~: check EEmpty p24Vuelta f24Vuelta ~?= CheckOK
        , "alphaEq E x. A(x) => E y. A(y) directo"
            ~: check
                EEmpty
                (PImpI "h E x. A(x)" (PAx "h E x. A(x)"))
                ( FImp
                    (FExists "x" (predVar "A" "x"))
                    (FExists "y" (predVar "A" "y"))
                )
            ~?= CheckOK
        , "subst sin captura - E y. V x. A(z) v true"
            ~: check
                EEmpty
                ( PExistsI
                    (TVar "x") -- generaria captura con V x
                    (PForallI (POrI2 PTrueI))
                )
                (FExists "y" (FForall "x" (FOr (predVar "A" "z") FTrue)))
            ~?= CheckOK
        ]
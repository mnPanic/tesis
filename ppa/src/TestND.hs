module TestND (testND) where

import ND (
    Env (..),
    Form (..),
    HypId,
    PredId,
    Proof (..),
    Term (..),
    VarId,
    dneg,
    fPred1,
    fv,
    fvE,
    get,
    predVar,
    propVar,
    tFun0,
    tFun1,
 )

import Certifier (
    dnf,
    fromClause,
    solveContradiction,
 )

import NDChecker (
    CheckResult (CheckError, CheckErrorN, CheckOK),
    check,
    rootCause,
    subst,
 )

import NDProofs (
    Result,
    cut,
    doubleNegElim,
    hypForm,
    proofAndAssoc,
    proofAndCongruence1,
    proofAndCongruence2,
    proofAndDistOverOrL,
    proofAndDistOverOrR,
    proofAndEProjection,
    proofAndIList,
    proofDNegElim,
    proofImpCongruence1,
    proofImpCongruence2,
    proofImpElim,
    proofNotCongruence,
    proofNotDistOverAnd,
    proofNotDistOverOr,
    proofNotFalse,
    proofNotTrue,
    proofOrAssoc,
    proofOrCongruence1,
    proofOrCongruence2,
 )

import NDReducer (reduce, substHyp)

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

import Data.Map qualified as Map
import Data.Set qualified as Set
import Unifier (Substitution, unifyF, unifyT)

main :: IO Counts
main = do runTestTT testND

testND :: Test
testND =
    test
        [ "check" ~: testCheckExamples
        , "gen proofs" ~: testGeneratedProofs
        , "env" ~: testEnv
        , "subst" ~: testSubst
        , "fv" ~: testFV
        , "alphaEq" ~: testAlphaEq
        , "unify" ~: testUnify
        , "reduce" ~: testReduce
        , "substHyp" ~: testSubstHyp
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

testUnify :: Test
testUnify =
    test
        [ "terms"
            ~: test
                [ "equal vars"
                    ~: unifyF Map.empty (fPred1 "p" (TVar "x")) (fPred1 "p" (TVar "x"))
                    ~?= Right Map.empty
                , "diff vars"
                    ~: unifyF Map.empty (fPred1 "p" (TVar "x")) (fPred1 "p" (TVar "y"))
                    ~?= Left "different var names: x /= y"
                , "diff fun names"
                    ~: unifyF Map.empty (fPred1 "p" (tFun0 "f")) (fPred1 "p" (tFun0 "g"))
                    ~?= Left "different function names: f /= g"
                , "composed error"
                    ~: unifyF Map.empty (fPred1 "p" (TFun "g" [TVar "x"])) (fPred1 "p" (tFun1 "g" (TVar "y")))
                    ~?= Left "different var names: x /= y"
                , "unification OK"
                    ~: unifyF
                        Map.empty
                        (FPred "p" [TFun "g" [TVar "x", TMetavar 1, TVar "y"]])
                        (FPred "p" [TFun "g" [TVar "x", TFun "w" [TVar "z"], TVar "y"]])
                    ~?= Right (Map.singleton 1 (TFun "w" [TVar "z"]))
                ]
        , "complex form"
            ~: unifyF
                Map.empty
                ( FAnd
                    (FOr (predVar "p" "x") (FOr FTrue FFalse))
                    ( FImp
                        (FNot $ FPred "p" [TVar "x", TVar "g"])
                        ( FAnd
                            (FForall "x" (FPred "p" [TMetavar 1])) -- Puede capturar
                            (FExists "y" (predVar "q" "y"))
                        )
                    )
                )
                ( FAnd
                    (FOr (FPred "p" [TMetavar 1]) (FOr FTrue FFalse))
                    ( FImp
                        (FNot $ FPred "p" [TMetavar 1, TVar "g"])
                        ( FAnd
                            (FForall "x" (predVar "p" "x"))
                            (FExists "y" (predVar "q" "y"))
                        )
                    )
                )
            ~?= Right (Map.singleton 1 (TVar "x"))
        , "and with bool"
            ~: unifyF
                Map.empty
                (FAnd (fPred1 "p" $ TVar "x") FTrue)
                (FAnd (fPred1 "p" $ TMetavar 1) FTrue)
            ~?= Right (Map.singleton 1 (TVar "x"))
        , "error different forms"
            ~: unifyF
                Map.empty
                (FPred "p" [])
                FTrue
            ~?= Left "different form types: p /= true"
        , "error don't unify"
            ~: unifyF
                Map.empty
                (FAnd (predVar "f" "x") (FPred "g" [TMetavar 1]))
                (FAnd (FPred "f" [TMetavar 1]) (predVar "g" "y"))
            ~?= Left "different var names: x /= y"
        , "preds"
            ~: unifyF Map.empty (FPred "p" [TMetavar 1]) (FPred "p" [TVar "x"])
            ~?= Right (Map.singleton 1 (TVar "x"))
        , "more than one metavar"
            ~: unifyF
                Map.empty
                (FImp (fPred1 "p" (TMetavar 1)) (FAnd (fPred1 "q" (TFun "f" [TVar "x", TMetavar 2])) (fPred1 "r" (TMetavar 3))))
                (FImp (fPred1 "p" (tFun1 "f" (TVar "y"))) (FAnd (fPred1 "q" (TFun "f" [TVar "x", TVar "z"])) (fPred1 "r" (TVar "x"))))
            ~?= Right
                ( Map.fromList
                    [ (1, tFun1 "f" (TVar "y"))
                    , (2, TVar "z")
                    , (3, TVar "x")
                    ]
                )
        , "alpha eq"
            ~: test
                [ "ok"
                    ~: unifyF
                        Map.empty
                        (FForall "z" (FAnd (fPred1 "p" (TVar "z")) (fPred1 "g" (TMetavar 1))))
                        (FForall "y" (FAnd (fPred1 "p" (TVar "y")) (fPred1 "g" (tFun0 "a"))))
                    ~?= Right (Map.singleton 1 (tFun0 "a"))
                , "err capture simple single var"
                    ~: unifyF
                        Map.empty
                        (FForall "x" (fPred1 "p" (TMetavar 1)))
                        (FForall "y" (predVar "p" "y"))
                    ~?= Left "Rename check: 'y' has free variables that were renamed for alpha equivalence"
                , "err capture func"
                    ~: unifyF
                        Map.empty
                        (FForall "x" (fPred1 "p" (TMetavar 1)))
                        (FForall "y" (fPred1 "p" (tFun1 "f" (TVar "y"))))
                    ~?= Left "Rename check: 'f(y)' has free variables that were renamed for alpha equivalence"
                , "err capture full"
                    ~: unifyF
                        Map.empty
                        (FForall "y" (FAnd (FForall "x" (fPred1 "p" (TMetavar 1))) (predVar "g" "x")))
                        (FForall "x" (FAnd (FForall "x" (fPred1 "p" (TVar "x"))) (fPred1 "g" (TMetavar 1))))
                    ~?= Left "Rename check: 'x' has free variables that were renamed for alpha equivalence"
                ]
        ]

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
        , -- Doble cuantificador. Aunque no haya ningún reemplazo que hacer, alpha
          -- renombra igual
          subst
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
        , subst
            "z"
            (TFun "f" [TVar "x", TVar "y"])
            ( FForall
                "x"
                ( FAnd
                    ( FAnd
                        (FPred "P" [TVar "x"])
                        (FForall "y" (FPred "Q" [TVar "y"]))
                    )
                    (FPred "R" [TVar "z"])
                )
            )
            ~?= FForall
                "x0"
                ( FAnd
                    ( FAnd
                        (FPred "P" [TVar "x0"])
                        (FForall "y0" (FPred "Q" [TVar "y0"]))
                    )
                    (FPred "R" [TFun "f" [TVar "x", TVar "y"]])
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

testCheckExamples :: Test
testCheckExamples =
    test
        [ -- PAx
          "A |- A" ~: check exampleEnv (PAx "h1") FTrue ~?= CheckOK
        , "A |- B invalid"
            ~: rootCause (check exampleEnv (PAx "h1") FFalse)
            ~?= CheckError exampleEnv (PAx "h1") FFalse "env has hyp 'h1' for different form 'true'"
        , "PAx alpha eq"
            ~: check
                (EExtend "h" (FExists "x" (predVar "p" "x")) EEmpty)
                (PAx "h")
                (FExists "y" (predVar "p" "y"))
            ~?= CheckOK
        , -- PImpI
          "A -> A" ~: check EEmpty p1 f1 ~?= CheckOK
        , "A -> (B -> A)" ~: check EEmpty p2 f2 ~?= CheckOK
        , -- Usar la misma etiqueta para diferentes hipótesis
          "A -> (B -> B)" ~: check EEmpty p3 f3 ~?= CheckOK
        , "A -> (B -> A) invalid"
            ~: rootCause (check EEmpty p3 f2)
            ~?= CheckError
                (EExtend "x" (FPred "B" []) (EExtend "x" (FPred "A" []) EEmpty))
                (PAx "x")
                (FPred "A" [])
                "env has hyp 'x' for different form 'B'"
        , -- PImpE
          "(A -> (B -> C)) -> [(A -> B) -> (A -> C)]"
            ~: check EEmpty p4 f4
            ~?= CheckOK
        , "(A -> (B -> C)) -> [(A -> B) -> (A -> C)] err left"
            ~: rootCause (check EEmpty p4Err1 f4)
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
            ~: rootCause (check EEmpty p4Err2 f4)
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
        , "Exists x. A(x) ^ B(x) => Exists y. A(y) con renombre"
            ~: check EEmpty p19_rename f19
            ~?= CheckOK
        , "Forall x. A(x) ^ B(x) => Forall x. A(x)" ~: check EEmpty p20 f20 ~?= CheckOK
        , "Forall x. A(x) ^ B(x) => Forall x. A(x) with rename in forallI"
            ~: check EEmpty p20_2 f20
            ~?= CheckOK
        , "Forall x. A(x) ^ B(x) => Forall y. A(y)" ~: check EEmpty p20' f20' ~?= CheckOK
        , "Forall x. A(x) => Exists x. B(x)"
            ~: rootCause (check EEmpty p22 f22)
            ~?= CheckError
                (EExtend "h Forall x. A(x)" (FForall "x" (FPred "A" [TVar "x"])) EEmpty)
                (PForallE "x" (FPred "A" [TVar "x"]) (PAx "h Forall x. A(x)") (TVar "x"))
                (FPred "B" [TVar "x"])
                "form B(x) /= (A(x)){x := x}"
        , "A(x) => Forall x. A(x)"
            ~: rootCause (check EEmpty p21 f21)
            ~?= CheckError
                (EExtend "h A(x)" (FPred "A" [TVar "x"]) EEmpty)
                (PForallI "x" (PAx "h A(x)"))
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
                    (PForallI "x" (POrI2 PTrueI))
                )
                (FExists "y" (FForall "x" (FOr (predVar "A" "z") FTrue)))
            ~?= CheckOK
        ]

-- generated proofs
testGeneratedProofs :: Test
testGeneratedProofs =
    test
        [ "examples for by" ~: testByExamples
        , "andEProjection" ~: testAndEProjection
        , "PAndI list" ~: testAndIList
        , "equivalences" ~: testEquivalences
        ]

testAndIList :: Test
testAndIList = do
    let form = fromClause [propVar "A", propVar "B", propVar "C"]
    let subproofs = [PAx "a", PAx "b", PAx "c"]
    let env = EExtend "a" (propVar "A") (EExtend "b" (propVar "B") (EExtend "c" (propVar "C") EEmpty))
    let proof = proofAndIList subproofs
    check env proof form ~?= CheckOK

testAndEProjection :: Test
testAndEProjection =
    test
        [ "A ^ B |- B"
            ~: testAndEProj
                (FAnd (propVar "A") (propVar "B"))
                "h A ^ B"
                (propVar "B")
                PAndE2
                    { left = propVar "A"
                    , proofAnd = PAx "h A ^ B"
                    }
        , "(A ^ B) ^ C |- B"
            ~: testAndEProj
                (FAnd (FAnd (propVar "A") (propVar "B")) (propVar "C"))
                "h (A ^ B) ^ C"
                (propVar "B")
                PAndE2
                    { left = propVar "A"
                    , proofAnd =
                        PAndE1
                            { right = propVar "C"
                            , proofAnd = PAx "h (A ^ B) ^ C"
                            }
                    }
        , "A ^ (B ^ C) |- B"
            ~: testAndEProj
                (FAnd (propVar "A") (FAnd (propVar "B") (propVar "C")))
                "h A ^ (B ^ C)"
                (propVar "B")
                PAndE1
                    { right = propVar "C"
                    , proofAnd =
                        PAndE2
                            { left = propVar "A"
                            , proofAnd = PAx "h A ^ (B ^ C)"
                            }
                    }
        , "((A ^ (B ^ C)) ^ D) ^ E |- B"
            ~: testAndEProj
                ( FAnd
                    ( FAnd
                        ( FAnd
                            (propVar "A")
                            (FAnd (propVar "B") (propVar "C"))
                        )
                        (propVar "D")
                    )
                    (propVar "E")
                )
                "h And"
                (propVar "B")
                PAndE1
                    { right = propVar "C"
                    , proofAnd =
                        PAndE2
                            { left = propVar "A"
                            , proofAnd =
                                PAndE1
                                    { right = propVar "D"
                                    , proofAnd =
                                        PAndE1
                                            { right = propVar "E"
                                            , proofAnd = PAx "h And"
                                            }
                                    }
                            }
                    }
        , "(A ^ B) ^ C |- A ^ B"
            ~: testAndEProj
                (FAnd (FAnd (propVar "A") (propVar "B")) (propVar "C"))
                "h (A ^ B) ^ C"
                (FAnd (propVar "A") (propVar "B"))
                PAndE1
                    { right = propVar "C"
                    , proofAnd = PAx "h (A ^ B) ^ C"
                    }
        , "mixta - ((C => D) ^ (A v B)) ^ ~C |- A v B"
            ~: testAndEProj
                ( FAnd
                    ( FAnd
                        (FImp (propVar "C") (propVar "D"))
                        (FOr (propVar "A") (propVar "B"))
                    )
                    (FNot $ propVar "C")
                )
                "h And"
                (FOr (propVar "A") (propVar "B"))
                PAndE2
                    { left = FImp (propVar "C") (propVar "D")
                    , proofAnd =
                        PAndE1
                            { right = FNot $ propVar "C"
                            , proofAnd = PAx "h And"
                            }
                    }
        , "err A & B |- C"
            ~: proofAndEProjection ("h", FAnd (propVar "A") (propVar "B")) (propVar "C")
            ~?= Left "A & B |- C not possible by left (A /= C) or right (B /= C)"
        , "err ((A & (B & C)) & D) & E |- Q"
            ~: proofAndEProjection
                ( "h"
                , FAnd
                    ( FAnd
                        ( FAnd
                            (propVar "A")
                            (FAnd (propVar "B") (propVar "C"))
                        )
                        (propVar "D")
                    )
                    (propVar "E")
                )
                (propVar "Q")
            ~?= Left "((A & (B & C)) & D) & E |- Q not possible by left ((A & (B & C)) & D |- Q not possible by left (A & (B & C) |- Q not possible by left (A /= Q) or right (B & C |- Q not possible by left (B /= Q) or right (C /= Q))) or right (D /= Q)) or right (E /= Q)"
        ]

testAndEProj :: Form -> HypId -> Form -> Proof -> IO ()
testAndEProj fAnd hAnd f expectedProof = do
    let result = proofAndEProjection (hAnd, fAnd) f
    result @?= Right expectedProof
    let (Right proof) = result
    check (EExtend hAnd fAnd EEmpty) proof f @?= CheckOK

-- Test de demostraciones necesarias para la implementación de by
testByExamples :: Test
testByExamples =
    test
        [ "X & (Y v Z) -> (X & Y) v (X & Z)"
            ~: check EEmpty p25' f25'
            ~?= CheckOK
        , "(X & Y) v (X & Z) -> X & (Y v Z) with macro"
            ~: check EEmpty (proofDistOrOverAnd (propVar "X") (propVar "Y") (propVar "Z")) f25
            ~?= CheckOK
        , -- andEProj
          "trans no proj ((A -> B) & (B -> C)) & A -> C"
            ~: check EEmpty p27 f27
            ~?= CheckOK
        , "trans w/ andEProj ((A -> B) & (B -> C)) & A -> C"
            ~: case p27_andEProjection of
                (Left e) -> assertFailure e
                (Right p) -> check EEmpty p f27 @?= CheckOK
        , "example solve contradiction manually (A & ~A & ~B) v (A & B & ~B) -> false (bot)" ~: case p28_exampleSolve of
            (Left e) -> assertFailure e
            (Right p) -> check EEmpty p f28_exampleSolve @?= CheckOK
            -- , "by manually: ( ( A ^ (A -> B) ) -> B )" f26 p26
        ]

-- Test de demostraciones de equivalencias necesarias para dnf
testEquivalences :: Test
testEquivalences =
    test
        [ "not true" ~: do
            let (hNotTrue, hFalse) = (hypForm (FNot FTrue), hypForm FFalse)
            let (pLR, pRL) = proofNotTrue hNotTrue hFalse
            checkEquiv hNotTrue (FNot FTrue) hFalse FFalse pLR pRL
        , "not false" ~: do
            let (hNotFalse, hTrue) = (hypForm (FNot FFalse), hypForm FTrue)
            let (pLR, pRL) = proofNotFalse hNotFalse hTrue
            checkEquiv hNotFalse (FNot FFalse) hTrue FTrue pLR pRL
        , "imp elim" ~: do
            let (x, y) = (propVar "X", propVar "Y")
            let (fImp, fOr) = (FImp x y, FOr (FNot x) y)
            let (hImp, hOr) = (hypForm fImp, hypForm fOr)
            let (pImpElim, pOrToImp) = proofImpElim x y hImp hOr
            checkEquiv hImp fImp hOr fOr pImpElim pOrToImp
        , "not dist over and" ~: do
            let (x, y) = (propVar "X", propVar "Y")
            let (fNotAnd, fOrNots) = (FNot $ FAnd x y, FOr (FNot x) (FNot y))
            let (hNotAnd, hOrNots) = (hypForm fNotAnd, hypForm fOrNots)
            let (pLR, pRL) = proofNotDistOverAnd x y hNotAnd hOrNots
            checkEquiv hNotAnd fNotAnd hOrNots fOrNots pLR pRL
        , "not dist over or" ~: do
            let (x, y) = (propVar "X", propVar "Y")
            let (fNotOr, fAndNots) = (FNot $ FOr x y, FAnd (FNot x) (FNot y))
            let (hNotOr, hAndNots) = (hypForm fNotOr, hypForm fAndNots)
            let (pLR, pRL) = proofNotDistOverOr x y hNotOr hAndNots
            checkEquiv hNotOr fNotOr hAndNots fAndNots pLR pRL
        , "dneg elim" ~: do
            let x = propVar "X"
            let dnegX = dneg x
            let (hX, hDNegX) = (hypForm x, hypForm dnegX)
            let (pDNegE, pDNegI) = proofDNegElim x hX hDNegX
            checkEquiv hX x hDNegX dnegX pDNegI pDNegE
        , "or assoc" ~: do
            let (x, y, z) = (propVar "x", propVar "y", propVar "z")
            let (fOrL, fOrR) = (FOr (FOr x y) z, FOr x (FOr y z))
            let (hOrL, hOrR) = (hypForm fOrL, hypForm fOrR)
            let (pOrAssocLR, pOrAssocRL) = proofOrAssoc x y z hOrL hOrR
            checkEquiv hOrL fOrL hOrR fOrR pOrAssocLR pOrAssocRL
        , "and assoc" ~: do
            let (x, y, z) = (propVar "x", propVar "y", propVar "z")
            let (fAndL, fAndR) = (FAnd (FAnd x y) z, FAnd x (FAnd y z))
            let (hAndL, hAndR) = (hypForm fAndL, hypForm fAndR)
            let (pAndAssocLR, pAndAssocRL) = proofAndAssoc x y z hAndL hAndR
            checkEquiv hAndL fAndL hAndR fAndR pAndAssocLR pAndAssocRL
        , "and dist over or L" ~: do
            let (x, y, z) = (propVar "x", propVar "y", propVar "z")
            let fAnd = FAnd x (FOr y z)
            let fOr = FOr (FAnd x y) (FAnd x z)
            let (hAnd, hOr) = (hypForm fAnd, hypForm fOr)
            let (pLR, pRL) = proofAndDistOverOrL x y z hAnd hOr
            checkEquiv hAnd fAnd hOr fOr pLR pRL
        , "and dist over or R" ~: do
            let (x, y, z) = (propVar "x", propVar "y", propVar "z")
            let fAnd = FAnd (FOr y z) x
            let fOr = FOr (FAnd y x) (FAnd z x)
            let (hAnd, hOr) = (hypForm fAnd, hypForm fOr)
            let (pLR, pRL) = proofAndDistOverOrR x y z hAnd hOr
            checkEquiv hAnd fAnd hOr fOr pLR pRL
        , "and congruence 1"
            ~: do
                -- if X => Y -|- ~X v Y then (X => Y) ^ Z -|- (~X v Y) ^ Z
                let (x, y, z) = (propVar "X", propVar "Y", propVar "Z")
                let fImp = FImp x y
                let fAnd = FAnd fImp z
                let (hAnd, hImp) = (hypForm fAnd, hypForm fImp)
                -- in DNF
                let fOr = FOr (FNot x) y
                let fAnd' = FAnd fOr z
                let (hAnd', hOr) = (hypForm fAnd', hypForm fOr)
                let (pImpElimLR, pImpElimRL) = proofImpElim x y hImp hOr

                let (pCongLR, pCongRL) = proofAndCongruence1 fImp z fOr hAnd hAnd' hImp pImpElimLR hOr pImpElimRL
                CheckOK @=? check (EExtend hAnd fAnd EEmpty) pCongLR fAnd'
                CheckOK @=? check (EExtend hAnd' fAnd' EEmpty) pCongRL fAnd
        , "and congruence 2"
            ~: do
                -- if X => Y -|- ~X v Y then Z ^ (X => Y) -|- Z ^ (~X v Y)
                let (x, y, z) = (propVar "X", propVar "Y", propVar "Z")
                let fImp = FImp x y
                let fAnd = FAnd z fImp
                let (hAnd, hImp) = (hypForm fAnd, hypForm fImp)
                -- in DNF
                let fOr = FOr (FNot x) y
                let fAnd' = FAnd z fOr
                let (hAnd', hOr) = (hypForm fAnd', hypForm fOr)
                let (pImpElimLR, pImpElimRL) = proofImpElim x y hImp hOr

                let (pCongLR, pCongRL) = proofAndCongruence2 z fImp fOr hAnd hAnd' hImp pImpElimLR hOr pImpElimRL
                CheckOK @=? check (EExtend hAnd fAnd EEmpty) pCongLR fAnd'
                CheckOK @=? check (EExtend hAnd' fAnd' EEmpty) pCongRL fAnd
        , "or congruence 1"
            ~: do
                -- if X => Y -|- ~X v Y then (X => Y) v Z -|- (~X v Y) v Z
                let (x, y, z) = (propVar "X", propVar "Y", propVar "Z")
                let left = FImp x y
                let f = FOr left z

                let left' = FOr (FNot x) y
                let f' = FOr left' z
                let (hF, hF') = (hypForm f, hypForm f')
                let (hL, hL') = (hypForm left, hypForm left')

                let (pLL', pL'L) = proofImpElim x y hL hL'

                let (pCongLR, pCongRL) = proofOrCongruence1 left z left' hF hF' hL pLL' hL' pL'L
                CheckOK @=? check (EExtend hF f EEmpty) pCongLR f'
                CheckOK @=? check (EExtend hF' f' EEmpty) pCongRL f
        , "or congruence 2"
            ~: do
                -- if X => Y -|- ~X v Y then Z v (X => Y) -|- Z v (~X v Y)
                let (x, y, z) = (propVar "X", propVar "Y", propVar "Z")
                let right = FImp x y
                let f = FOr z right

                let right' = FOr (FNot x) y
                let f' = FOr z right'
                let (hF, hF') = (hypForm f, hypForm f')
                let (hR, hR') = (hypForm right, hypForm right')

                let (pRR', pR'R) = proofImpElim x y hR hR'

                let (pCongLR, pCongRL) = proofOrCongruence2 z right right' hF hF' hR pRR' hR' pR'R
                CheckOK @=? check (EExtend hF f EEmpty) pCongLR f'
                CheckOK @=? check (EExtend hF' f' EEmpty) pCongRL f
        , "not congruence"
            ~: do
                let (x, y) = (propVar "X", propVar "Y")
                let (f, f') = (FImp x y, FOr (FNot x) y)
                let (hF, hF') = (hypForm f, hypForm f')
                let (pFF', pF'F) = proofImpElim x y hF hF'

                let (fNot, fNot') = (FNot f, FNot f')
                let (hNot, hNot') = (hypForm fNot, hypForm fNot')

                let (pCongLR, pCongRL) = proofNotCongruence f f' hNot hNot' hF pFF' hF' pF'F
                CheckOK @=? check (EExtend hNot fNot EEmpty) pCongLR fNot'
                CheckOK @=? check (EExtend hNot' fNot' EEmpty) pCongRL fNot
        , "imp congruence 1"
            ~: do
                let (x, y, z) = (propVar "X", propVar "Y", propVar "Z")
                let (f, f') = (FImp x y, FOr (FNot x) y)
                let (hF, hF') = (hypForm f, hypForm f')
                let (pFF', pF'F) = proofImpElim x y hF hF'

                let (fImp, fImp') = (FImp f z, FImp f' z)
                let (hImp, hImp') = (hypForm fImp, hypForm fImp')

                let (pCongLR, pCongRL) = proofImpCongruence1 f z f' hImp hImp' hF pFF' hF' pF'F
                CheckOK @=? check (EExtend hImp fImp EEmpty) pCongLR fImp'
                CheckOK @=? check (EExtend hImp' fImp' EEmpty) pCongRL fImp
        , "imp congruence 2"
            ~: do
                let (x, y, z) = (propVar "X", propVar "Y", propVar "Z")
                let (f, f') = (FImp x y, FOr (FNot x) y)
                let (hF, hF') = (hypForm f, hypForm f')
                let (pFF', pF'F) = proofImpElim x y hF hF'

                let (fImp, fImp') = (FImp z f, FImp z f')
                let (hImp, hImp') = (hypForm fImp, hypForm fImp')

                let (pCongLR, pCongRL) = proofImpCongruence2 z f f' hImp hImp' hF pFF' hF' pF'F
                CheckOK @=? check (EExtend hImp fImp EEmpty) pCongLR fImp'
                CheckOK @=? check (EExtend hImp' fImp' EEmpty) pCongRL fImp
        ]

checkEquiv :: HypId -> Form -> HypId -> Form -> Proof -> Proof -> IO ()
checkEquiv hF f hF' f' pFThenF' pF'ThenF = do
    CheckOK @=? check (EExtend hF f EEmpty) pFThenF' f'
    CheckOK @=? check (EExtend hF' f' EEmpty) pF'ThenF f

testSubstHyp :: Test
testSubstHyp =
    test
        [ "simple"
            ~: substHyp
                "h"
                (PAx "b")
                (PAx "h")
            ~?= PAx "b"
        , "capture"
            ~: substHyp
                "h"
                ( PAndE1
                    { right = propVar "a"
                    , proofAnd = PAx "q"
                    }
                )
                ( PImpI
                    { hypAntecedent = "q" -- son qs diferentes
                    , proofConsequent =
                        PAndI
                            { proofLeft = PAx "h"
                            , proofRight = PAx "q"
                            }
                    }
                )
            ~?= ( PImpI
                    { hypAntecedent = "q0"
                    , proofConsequent =
                        PAndI
                            { proofLeft =
                                ( PAndE1
                                    { right = propVar "a"
                                    , proofAnd = PAx "q"
                                    }
                                )
                            , proofRight = PAx "q0"
                            }
                    }
                )
        ]

testReduce :: Test
testReduce =
    test
        [ "and"
            ~: test
                [ "left"
                    ~: doTestReduce
                        (EExtend "h1" (propVar "p") $ EExtend "h2" (propVar "q") EEmpty)
                        (propVar "p")
                        ( PAndE1
                            { right = propVar "q"
                            , proofAnd =
                                ( PAndI
                                    { proofLeft = PAx "h1"
                                    , proofRight = PAx "h2"
                                    }
                                )
                            }
                        )
                        (PAx "h1")
                , "right"
                    ~: doTestReduce
                        (EExtend "h1" (propVar "p") $ EExtend "h2" (propVar "q") EEmpty)
                        (propVar "q")
                        ( PAndE2
                            { left = propVar "p"
                            , proofAnd =
                                ( PAndI
                                    { proofLeft = PAx "h1"
                                    , proofRight = PAx "h2"
                                    }
                                )
                            }
                        )
                        (PAx "h2")
                , "left closed"
                    ~: do
                        let (p, q) = (propVar "p", propVar "q")
                        doTestReduce
                            EEmpty
                            (FImp p (FImp q p))
                            ( PImpI
                                { hypAntecedent = "h1"
                                , proofConsequent =
                                    PImpI
                                        { hypAntecedent = "h2"
                                        , proofConsequent =
                                            PAndE1
                                                { right = propVar "q"
                                                , proofAnd =
                                                    ( PAndI
                                                        { proofLeft = PAx "h1"
                                                        , proofRight = PAx "h2"
                                                        }
                                                    )
                                                }
                                        }
                                }
                            )
                            ( PImpI
                                { hypAntecedent = "h1"
                                , proofConsequent =
                                    PImpI
                                        { hypAntecedent = "h2"
                                        , proofConsequent = PAx "h1"
                                        }
                                }
                            )
                ]
        , "or"
            ~: test
                [ "l" ~: do
                    let (p, q) = (propVar "p", propVar "q")
                    doTestReduce
                        (EExtend "q imp p" (FImp q p) EEmpty)
                        (FImp p p)
                        ( PImpI
                            { hypAntecedent = "p"
                            , proofConsequent =
                                POrE
                                    { left = p
                                    , right = q
                                    , proofOr = POrI1 (PAx "p")
                                    , hypLeft = "p"
                                    , proofAssumingLeft = PAx "p"
                                    , hypRight = "q"
                                    , proofAssumingRight =
                                        PImpE
                                            { antecedent = q
                                            , proofImp = PAx "q imp p"
                                            , proofAntecedent = PAx "q"
                                            }
                                    }
                            }
                        )
                        ( PImpI
                            { hypAntecedent = "p"
                            , proofConsequent = PAx "p"
                            }
                        )
                , "r"
                    ~: do
                        let (p, q) = (propVar "p", propVar "q")
                        doTestReduce
                            (EExtend "q imp p" (FImp q p) EEmpty)
                            (FImp p p)
                            ( PImpI
                                { hypAntecedent = "p"
                                , proofConsequent =
                                    POrE
                                        { left = q
                                        , right = p
                                        , proofOr = POrI2 (PAx "p")
                                        , hypLeft = "q"
                                        , proofAssumingLeft =
                                            PImpE
                                                { antecedent = q
                                                , proofImp = PAx "q imp p"
                                                , proofAntecedent = PAx "q"
                                                }
                                        , hypRight = "p"
                                        , proofAssumingRight = PAx "p"
                                        }
                                }
                            )
                            ( PImpI
                                { hypAntecedent = "p"
                                , proofConsequent = PAx "p"
                                }
                            )
                ]
        ]

doTestReduce :: Env -> Form -> Proof -> Proof -> Assertion
doTestReduce env f p expectedP = do
    assertEqual "original doesn't check" CheckOK (check env p f)
    let p' = reduce p
    expectedP @=? p'
    assertEqual "reduced doesn't check" CheckOK (check env p' f)

{-                                  Proofs                                    -}

-- Dems sacadas de ejercicios de Lectures on the Curry Howard Isomorphism
-- Originalmente son para deducción natural de intuicionista.

-- A -> A
f1 :: Form
f1 = FImp (propVar "A") (propVar "A")

p1 :: Proof
p1 = PImpI "hA" (PAx "hA")

-- A -> (B -> A)
f2 :: Form
f2 =
    FImp
        (propVar "A")
        ( FImp
            (propVar "B")
            (propVar "A")
        )

p2 :: Proof
p2 = PImpI "hA" (PImpI "hB" (PAx "hA"))

-- A -> (B -> B)
f3 :: Form
f3 =
    FImp
        (propVar "A")
        ( FImp
            (propVar "B")
            (propVar "B")
        )

p3 :: Proof
p3 = PImpI "x" (PImpI "x" (PAx "x"))

-- (A -> (B -> C)) -> [(A -> B) -> (A -> C)]
f4 :: Form
f4 =
    FImp
        ( FImp
            (propVar "A")
            (FImp (propVar "B") (propVar "C"))
        )
        ( FImp
            (FImp (propVar "A") (propVar "B"))
            (FImp (propVar "A") (propVar "C"))
        )

p4 :: Proof
p4 =
    PImpI
        "h A -> (B -> C)"
        ( PImpI
            "h A -> B"
            ( PImpI
                "h A"
                ( -- B -> C
                  PImpE
                    (propVar "B")
                    -- Dem B -> C por A -> (B -> C)
                    ( PImpE
                        (propVar "A")
                        (PAx "h A -> (B -> C)")
                        (PAx "h A")
                    )
                    -- Dem B por A -> B
                    ( PImpE
                        (propVar "A")
                        (PAx "h A -> B")
                        (PAx "h A")
                    )
                )
            )
        )

-- Errores en ambos juicios
p4Err1 :: Proof
p4Err1 =
    PImpI
        "h A -> (B -> C)"
        ( PImpI
            "h A -> B"
            ( PImpI
                "h A"
                ( -- B -> C
                  PImpE
                    (propVar "B")
                    -- Dem B -> C errónea, no hay hyp
                    (PAx "h B -> C")
                    -- Dem B por A -> B
                    ( PImpE
                        (propVar "A")
                        (PAx "h A -> B")
                        (PAx "h A")
                    )
                )
            )
        )

p4Err2 :: Proof
p4Err2 =
    PImpI
        "h A -> (B -> C)"
        ( PImpI
            "h A -> B"
            ( PImpI
                "h A"
                ( -- B -> C
                  PImpE
                    (propVar "B")
                    -- Dem B -> C por A -> (B -> C)
                    ( PImpE
                        (propVar "A")
                        (PAx "h A -> (B -> C)")
                        (PAx "h A")
                    )
                    -- Dem B errónea, no hay hyp
                    (PAx "h B")
                )
            )
        )

-- bot -> P
f5 :: Form
f5 = FImp FFalse $ propVar "P"

p5 :: Proof
p5 =
    PImpI
        "h False"
        ( PFalseE (PAx "h False")
        )

-- p -> ~~p
f6 :: Form
f6 = FImp (propVar "P") (FNot $ FNot $ propVar "P")

p6 :: Proof
p6 =
    PImpI
        "h P"
        ( PNotI
            "h ~P"
            ( PNotE
                (propVar "P")
                (PAx "h ~P")
                (PAx "h P")
            )
        )

-- ~~~p -> ~p
f7 :: Form
f7 = FImp (FNot $ FNot $ FNot $ propVar "P") (FNot $ propVar "P")

p7 :: Proof
p7 =
    PImpI
        "h ~~~P"
        ( PNotI
            "h P"
            ( PNotE
                (FNot $ FNot $ propVar "P")
                -- ~~~P
                (PAx "h ~~~P")
                -- ~~P
                ( PNotI
                    "h ~P"
                    ( PNotE
                        (propVar "P")
                        (PAx "h ~P")
                        (PAx "h P")
                    )
                )
            )
        )

-- modus tollens
-- (ej7 curry howard) (A -> B) -> (~B -> ~A)
f8 :: Form
f8 =
    FImp
        ( FImp
            (propVar "A")
            (propVar "B")
        )
        ( FImp
            (FNot $ propVar "B")
            (FNot $ propVar "A")
        )

p8 :: Proof
p8 =
    PImpI
        "h A -> B"
        ( PImpI
            "h ~B"
            ( PNotI
                "h A"
                ( PNotE
                    (propVar "B")
                    (PAx "h ~B")
                    -- dem B con A -> B
                    ( PImpE
                        (propVar "A")
                        (PAx "h A -> B")
                        (PAx "h A")
                    )
                )
            )
        )

-- ~~A -> A, si vale para LK
f9 :: Form
f9 = FImp (FNot $ FNot $ propVar "A") (propVar "A")

p9 :: Proof
p9 =
    PImpI
        "h ~~A"
        ( -- Uso LEM de A v ~A
          POrE
            (propVar "A")
            (FNot $ propVar "A")
            PLEM
            -- Dem de A asumiendo A
            "h A"
            (PAx "h A")
            -- Dem de A asumiendo ~ A
            "h ~A"
            ( -- ~A y ~~A generan una contradicción
              PFalseE
                ( PNotE
                    (FNot $ propVar "A") -- Uso ~~A
                    -- Dem de ~~A
                    (PAx "h ~~A")
                    -- Dem de ~A
                    (PAx "h ~A")
                )
            )
        )

-- De morgan

-- (ej9 CurryHoward) (~A v ~B) -> ~(A ^ B)

f10 :: Form
f10 =
    FImp
        (FOr (FNot $ propVar "A") (FNot $ propVar "B"))
        (FNot $ FAnd (propVar "A") (propVar "B"))

p10 :: Proof
p10 =
    PImpI
        "h ~A v ~B"
        ( PNotI
            "h A ^ B"
            ( -- Para demostrar ~(A^B), asumimos que no vale y dem false
              -- Para demostrar false, por casos en h ~A v ~B. En cualquiera
              -- llegamos a una contradicción con h A ^ B
              POrE
                (FNot $ propVar "A")
                (FNot $ propVar "B")
                (PAx "h ~A v ~B")
                "h ~A"
                ( PNotE
                    (propVar "A") -- uso ~A
                    (PAx "h ~A")
                    (PAndE1 (propVar "B") (PAx "h A ^ B"))
                )
                "h ~B"
                ( PNotE
                    (propVar "B") -- uso ~B
                    (PAx "h ~B")
                    (PAndE2 (propVar "A") (PAx "h A ^ B"))
                )
            )
        )

-- ej 11 CurryHoward, curryficación
-- ((A ^ B) -> C) <-> (A -> (B -> C))

f11 :: Form
f11 =
    FAnd
        ( FImp
            (FImp (FAnd (propVar "A") (propVar "B")) (propVar "C"))
            (FImp (propVar "A") (FImp (propVar "B") (propVar "C")))
        )
        ( FImp
            (FImp (propVar "A") (FImp (propVar "B") (propVar "C")))
            (FImp (FAnd (propVar "A") (propVar "B")) (propVar "C"))
        )

p11 :: Proof
p11 =
    PAndI
        -- ((A ^ B) -> C) -> (A -> (B -> C))
        ( PImpI
            "h (A ^ B) -> C)"
            ( PImpI
                "h A"
                ( PImpI
                    "h B"
                    ( PImpE
                        (FAnd (propVar "A") (propVar "B"))
                        (PAx "h (A ^ B) -> C)")
                        (PAndI (PAx "h A") (PAx "h B"))
                    )
                )
            )
        )
        -- (A -> (B -> C)) -> ((A ^ B) -> C)
        ( PImpI
            "h (A -> (B -> C))"
            ( PImpI
                "h A ^ B"
                ( -- Implico C a partir de B -> C que viene de A -> (B -> C)
                  PImpE
                    (propVar "B")
                    -- Interesante que la dem de B -> C no es PAx
                    ( PImpE
                        (propVar "A") -- A -> (B -> C)
                        (PAx "h (A -> (B -> C))")
                        (PAndE1 (propVar "B") (PAx "h A ^ B"))
                    )
                    (PAndE2 (propVar "A") (PAx "h A ^ B"))
                )
            )
        )

-- ej 13 CurryHoward
-- ~~(A v ~A)
f12 :: Form
f12 = FNot $ FNot $ FOr (propVar "A") (FNot $ propVar "A")

p12LEM :: Proof
p12LEM =
    PNotI
        "h ~(A v ~A)"
        ( PNotE
            (FOr (propVar "A") (FNot $ propVar "A"))
            -- ~(A v ~A)
            (PAx "h ~(A v ~A)")
            -- A v ~A
            -- medio trucho
            PLEM
        )

p12 :: Proof
p12 =
    PNotI
        "h ~(A v ~A)"
        ( PNotE
            (FOr (propVar "A") (FNot $ propVar "A"))
            -- ~(A v ~A)
            (PAx "h ~(A v ~A)")
            -- A v ~A
            ( POrI2
                ( PNotI
                    "h A"
                    ( PNotE
                        (FOr (propVar "A") (FNot $ propVar "A"))
                        (PAx "h ~(A v ~A)")
                        (POrI1 (PAx "h A"))
                    )
                )
            )
        )

-- alguna usando true
-- (A ^ true) <-> A
f13 :: Form
f13 =
    FAnd
        ( FImp
            (FAnd (propVar "A") FTrue)
            (propVar "A")
        )
        ( FImp
            (propVar "A")
            (FAnd (propVar "A") FTrue)
        )

p13 :: Proof
p13 =
    PAndI
        -- A ^ true -> A
        ( PImpI
            "h A ^ true"
            ( PAndE1 FTrue (PAx "h A ^ true")
            )
        )
        -- A -> A ^ true
        ( PImpI
            "h A"
            ( PAndI
                (PAx "h A")
                PTrueI
            )
        )

-- (A v true) <-> true
f14 :: Form
f14 =
    FAnd
        ( FImp
            (FOr (propVar "A") FTrue)
            FTrue
        )
        ( FImp
            FTrue
            (FOr (propVar "A") FTrue)
        )

p14 :: Proof
p14 =
    PAndI
        -- A v true -> true
        ( PImpI
            "h A v true"
            ( POrE
                (propVar "A")
                FTrue
                (PAx "h A v true")
                "h A"
                PTrueI
                "h true"
                PTrueI
            )
        )
        -- true -> A v true
        ( PImpI
            "h true"
            ( POrI2 $ PAx "h true"
            )
        )

-- vuelta (solo LK)
-- ~(A ^ B) -> (~A v ~B)
f15 :: Form
f15 =
    FImp
        (FNot $ FAnd (propVar "A") (propVar "B"))
        (FOr (FNot $ propVar "A") (FNot $ propVar "B"))

-- Estrategia: usar eliminación de la doble negación, y después se puede hacer
-- una dem intuicionista
-- Tengo una dem 100% clásica en el cuaderno pero es muy larga
p15 :: Proof
p15 =
    PImpI
        "h ~(A ^ B)"
        ( -- Uso eliminación de doble negación
          -- ~~(~A v ~B) -> ~A v ~B
          PImpE
            (dneg $ FOr (FNot fA) (FNot fB))
            (doubleNegElim $ FOr (FNot fA) (FNot fB))
            -- Dem de ~~(~A v ~B)
            ( PNotI
                "h ~(~A v ~B)"
                ( -- Acá es una dem por el absurdo, asumimos ~(~A v ~B) que es
                  -- la negación que lo que queremos probar, y llegamos a bottom.
                  PNotE
                    (FOr (FNot fA) (FNot fB))
                    (PAx "h ~(~A v ~B)")
                    ( POrI1
                        ( PNotI
                            "h A"
                            ( PNotE
                                (FOr (FNot fA) (FNot fB))
                                (PAx "h ~(~A v ~B)")
                                ( POrI2
                                    ( PNotI
                                        "h B"
                                        ( PNotE
                                            (FAnd fA fB)
                                            (PAx "h ~(A ^ B)")
                                            ( PAndI
                                                (PAx "h A")
                                                (PAx "h B")
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
  where
    fA = propVar "A"
    fB = propVar "B"

-- ~(A v B) <-> ~A ^ ~B

-- ~(A v B) -> ~A ^ ~B
f16 :: Form
f16 =
    FImp
        (FNot $ FOr fA fB)
        (FAnd (FNot fA) (FNot fB))
  where
    fA = propVar "A"
    fB = propVar "B"

p16 :: Proof
p16 =
    PImpI
        "h ~(A v B)"
        ( PAndI
            -- Dem ~A
            ( PNotI
                "h A"
                ( PNotE
                    (FOr fA fB)
                    (PAx "h ~(A v B)")
                    (POrI1 (PAx "h A"))
                )
            )
            -- Dem ~B
            ( PNotI
                "h B"
                ( PNotE
                    (FOr fA fB)
                    (PAx "h ~(A v B)")
                    (POrI2 (PAx "h B"))
                )
            )
        )
  where
    fA = propVar "A"
    fB = propVar "B"

-- ~A ^ ~B -> ~(A v B)
f17 :: Form
f17 =
    FImp
        (FAnd (FNot fA) (FNot fB))
        (FNot $ FOr fA fB)
  where
    fA = propVar "A"
    fB = propVar "B"

p17 :: Proof
p17 =
    PImpI
        "h ~A ^ ~B"
        ( PNotI
            "h A v B"
            ( -- dem de bottom (contradicción)
              -- idea: asumo A, por ~A contradicción. Análogo para B y ~B
              POrE
                fA
                fB
                (PAx "h A v B")
                "h A"
                -- proof de bottom asumiendo A
                ( PNotE
                    fA
                    (PAndE1 (FNot fB) (PAx "h ~A ^ ~B"))
                    (PAx "h A")
                )
                -- proof de bot asumiendo B
                "h B"
                ( PNotE
                    fB
                    (PAndE2 (FNot fA) (PAx "h ~A ^ ~B"))
                    (PAx "h B")
                )
            )
        )
  where
    fA = propVar "A"
    fB = propVar "B"

-- Dems bobas con exists y forall

-- Good(y) -> Exists x. Good(x)
f18 :: Form
f18 =
    FImp
        (predVar "Good" "y")
        (FExists "x" (predVar "Good" "x"))

p18 :: Proof
p18 =
    PImpI
        "h Good(y)"
        ( PExistsI
            (TVar "y")
            (PAx "h Good(y)")
        )

-- Exists x. A(x) ^ B(x) -> Exists y. A(y)
f19 :: Form
f19 =
    FImp
        ( FExists
            "x"
            ( FAnd
                (predVar "A" "x")
                (predVar "B" "x")
            )
        )
        (FExists "y" (predVar "A" "y"))

p19 :: Proof
p19 =
    PImpI
        "h Exists x. A(x) ^ B(x)"
        ( PExistsE
            "x"
            ( FAnd
                (predVar "A" "x")
                (predVar "B" "x")
            )
            (PAx "h Exists x. A(x) ^ B(x)")
            "h A(x) ^ B(x)"
            -- Dem Exists y. A(y)
            ( PExistsI
                (TVar "x")
                -- Dem A(x)
                ( PAndE1
                    (predVar "B" "x")
                    (PAx "h A(x) ^ B(x)")
                )
            )
        )

-- Exists x. A(x) ^ B(x) -> Exists y. A(y)
-- Banca renombres implícitos por alpha igualdad, en PAx Exists x. p(x)
-- demuestra Exists y . p(y)
p19_rename :: Proof
p19_rename =
    PImpI
        { hypAntecedent = "h Exists x. A(x) ^ B(x)"
        , proofConsequent =
            PExistsE
                { var = "y"
                , form =
                    FAnd
                        (predVar "A" "y")
                        (predVar "B" "y")
                , proofExists = PAx "h Exists x. A(x) ^ B(x)"
                , hyp = "h A(y) ^ B(y)"
                , proofAssuming -- Dem Exists y. A(y)
                  =
                    PExistsI
                        (TVar "y")
                        -- Dem A(y)
                        ( PAndE1
                            (predVar "B" "y")
                            (PAx "h A(y) ^ B(y)")
                        )
                }
        }

-- Es necesario primero hacer eliminación del exists y después PExistsI, PAndE1, porque sino al revés
-- se querría probar A(x) ^ B(x) con ExistsE pero no se puede porque tiene x libre.
-- Análogamente, para probar lo mismo pero del consecuente Exists x. A(x), no hay problema porque justamente x no está libre, sino que está ligado por el existencial.

-- Forall x. Good(x) -> Good(y)

-- Misma x, tiene que funcionar porque no está libre
-- Forall x. A(x) ^ B(x) => Forall x. A(x)
f20 :: Form
f20 =
    FImp
        (FForall "x" (FAnd ax bx))
        (FForall "x" ax)
  where
    ax = FPred "A" [TVar "x"]
    bx = FPred "B" [TVar "x"]

p20 :: Proof
p20 =
    PImpI
        "h Forall x. A(x) ^ B(x)"
        ( PForallI
            "x"
            ( -- Proof A(x)
              PAndE1
                bx
                ( PForallE
                    "x"
                    (FAnd ax bx)
                    (PAx "h Forall x. A(x) ^ B(x)")
                    (TVar "x")
                )
            )
        )
  where
    ax = FPred "A" [TVar "x"]
    bx = FPred "B" [TVar "x"]

p20_2 :: Proof
p20_2 =
    PImpI
        { hypAntecedent = "h forall"
        , proofConsequent =
            PForallI
                { newVar = "y"
                , proofForm =
                    PAndE1
                        { right = predVar "B" "y"
                        , proofAnd =
                            PForallE
                                { var = "x"
                                , form = FAnd (predVar "A" "x") (predVar "B" "x")
                                , proofForall = PAx "h forall"
                                , termReplace = TVar "y"
                                }
                        }
                }
        }

-- Var diferente, debería ser lo mismo
-- Forall x. A(x) ^ B(x) => Forall y. A(y)
f20' :: Form
f20' =
    FImp
        (FForall "x" (FAnd (predVar "A" "x") (predVar "B" "x")))
        (FForall "y" (predVar "A" "y"))

p20' :: Proof
p20' =
    PImpI
        "h Forall x. A(x) ^ B(x)"
        ( PForallI
            "y"
            ( -- Proof A(y)
              PAndE1
                by -- tengo que cambiar en ambos
                ( PForallE
                    "x"
                    (FAnd ax bx)
                    (PAx "h Forall x. A(x) ^ B(x)")
                    (TVar "y")
                )
            )
        )
  where
    ax = FPred "A" [TVar "x"]
    bx = FPred "B" [TVar "x"]
    by = FPred "B" [TVar "y"]

-- Dem inválida de introducción forall, en la que no está libre x en el contexto
-- A(x) => Forall x. A(x)
-- no está bien, y se podría demostrar con PAx
f21 :: Form
f21 =
    FImp
        (FPred "A" [TVar "x"])
        (FForall "x" (FPred "A" [TVar "x"]))

p21 :: Proof
p21 = PImpI "h A(x)" (PForallI "x" (PAx "h A(x)"))

-- Dem inválida de eliminación de forall, en donde el término a demostrar no
-- A{x := t} sino que otra cosa
-- Forall x. A(x) => Exists x. B(x)
f22 :: Form
f22 =
    FImp
        (FForall "x" $ FPred "A" [TVar "x"])
        (FExists "x" $ FPred "B" [TVar "x"])

p22 :: Proof
p22 =
    PImpI
        "h Forall x. A(x)"
        ( PExistsI
            (TVar "x")
            ( PForallE
                "x"
                (FPred "A" [TVar "x"])
                (PAx "h Forall x. A(x)")
                (TVar "x")
            )
        )

-- TODO leyes de demorgan (son dificiles - pablo)
-- 23 forall <=> ~exists~
-- 23' ~forall <=> exists~
-- 24 exists <=> ~forall~
-- 24' ~exists <=> forall~

-- V x. A(x) => ~ E x. ~A(x)
f23Ida :: Form
f23Ida =
    FImp
        (FForall "x" $ predVar "A" "x")
        (FNot $ FExists "x" $ FNot $ predVar "A" "x")

-- No se puede invertir el orden de NotE y ExistsE porque si hiciera NotE
-- primero, tendría que demostrar A(x) con ExistsE y no cumpliría con la
-- restricción de las FV

p23Ida :: Proof
p23Ida =
    PImpI
        "h V x. A(x)"
        ( PNotI
            "h E x. ~A(x)"
            ( -- Dem bot
              PExistsE
                "x"
                (FNot $ predVar "A" "x")
                (PAx "h E x. ~A(x)")
                "h ~A(x)"
                -- Contradicción de ~A(x) y A(x)
                ( PNotE
                    (predVar "A" "x")
                    (PAx "h ~A(x)")
                    -- Instancio V x. A(x) en x para llegar al abs
                    ( PForallE
                        "x"
                        (predVar "A" "x")
                        (PAx "h V x. A(x)")
                        (TVar "x") -- no cambia x
                    )
                )
            )
        )

-- Esta pinta que va a ser la difícil
-- ~E x. ~A(x) => V x. A(x)
f23Vuelta :: Form
f23Vuelta =
    FImp
        (FNot $ FExists "x" $ FNot $ predVar "A" "x")
        (FForall "x" $ predVar "A" "x")

p23Vuelta :: Proof
p23Vuelta =
    PImpI
        "h ~E x. ~A(x)"
        ( PForallI
            "x"
            ( -- Dem de A(x), por absurdo, asumo ~A(x) mediante dnegelim
              PImpE
                (dneg $ predVar "A" "x")
                (doubleNegElim $ predVar "A" "x")
                -- Dem ~~A(x)
                ( PNotI
                    "h ~A(x)"
                    ( PNotE
                        (FExists "x" $ FNot $ predVar "A" "x")
                        (PAx "h ~E x. ~A(x)")
                        -- Dem E x. ~A(x)
                        ( PExistsI
                            (TVar "x")
                            (PAx "h ~A(x)")
                        )
                    )
                )
            )
        )

-- E x. A(x) => ~ V x. ~A(x)
f24Ida :: Form
f24Ida =
    FImp
        (FExists "x" $ predVar "A" "x")
        (FNot $ FForall "x" $ FNot $ predVar "A" "x")

p24Ida :: Proof
p24Ida =
    PImpI
        "h E x. A(x)"
        ( PNotI
            "h V x. ~A(x)"
            ( PExistsE
                "x"
                (predVar "A" "x")
                (PAx "h E x. A(x)")
                "h A(x)"
                ( PNotE
                    (predVar "A" "x")
                    ( PForallE
                        "x"
                        (FNot $ predVar "A" "x")
                        (PAx "h V x. ~A(x)")
                        (TVar "x") -- queda igual
                    )
                    (PAx "h A(x)")
                )
            )
        )

-- ~ V x. ~A(x) => E x. A(x)
f24Vuelta :: Form
f24Vuelta =
    FImp
        (FNot $ FForall "x" $ FNot $ predVar "A" "x")
        (FExists "x" $ predVar "A" "x")

-- Acá no funciona primero hacer ExistsI y después dneg sobre A(x) porque el
-- absurdo es sobre la no existencia
p24Vuelta :: Proof
p24Vuelta =
    PImpI
        "h ~V x. ~A(x)"
        ( -- Dem E x. A(x) por absurdo mediante dneg elim, asumo ~E x. A(x)
          PImpE
            (dneg $ FExists "x" $ predVar "A" "x")
            (doubleNegElim $ FExists "x" $ predVar "A" "x")
            ( PNotI
                "h ~E x. A(x)"
                ( PNotE
                    (FForall "x" (FNot $ predVar "A" "x"))
                    (PAx "h ~V x. ~A(x)")
                    -- Dem de V x. ~A(x) (usando que no existe x. A(x))
                    ( PForallI
                        "x"
                        ( PNotI
                            "h A(x)"
                            ( PNotE
                                (FExists "x" $ predVar "A" "x")
                                (PAx "h ~E x. A(x)")
                                (PExistsI (TVar "x") (PAx "h A(x)"))
                            )
                        )
                    )
                )
            )
        )

-----
-- Demostraciones necesarias para el automatic proof del by

-- Cut, como la transitividad de la implicación
-- ((A => B) ^ (B => C)) ^ A => C
f27 :: Form
f27 =
    FImp
        ( FAnd
            ( FAnd
                (FImp (propVar "A") (propVar "B"))
                (FImp (propVar "B") (propVar "C"))
            )
            (propVar "A")
        )
        (propVar "C")

p27 :: Proof
p27 =
    PImpI
        { hypAntecedent = "h ((A => B) ^ (B => C)) ^ A"
        , -- Dem de C por transitividad
          proofConsequent =
            PImpE
                { antecedent = propVar "B"
                , -- Dem de B => C hay que meterse en el AND
                  proofImp =
                    PAndE2
                        { left = FImp (propVar "A") (propVar "B")
                        , proofAnd =
                            PAndE1
                                { right = propVar "A"
                                , proofAnd = PAx "h ((A => B) ^ (B => C)) ^ A"
                                }
                        }
                , -- Dem de B
                  proofAntecedent =
                    PImpE
                        { antecedent = propVar "A"
                        , proofImp =
                            PAndE1
                                { right = FImp (propVar "B") (propVar "C")
                                , proofAnd =
                                    PAndE1
                                        { right = propVar "A"
                                        , proofAnd = PAx "h ((A => B) ^ (B => C)) ^ A"
                                        }
                                }
                        , proofAntecedent =
                            PAndE2
                                { left =
                                    FAnd
                                        (FImp (propVar "A") (propVar "B"))
                                        (FImp (propVar "B") (propVar "C"))
                                , proofAnd = PAx "h ((A => B) ^ (B => C)) ^ A"
                                }
                        }
                }
        }

p27_andEProjection :: Result Proof
p27_andEProjection = do
    let ands =
            ( "h ((A => B) ^ (B => C)) ^ A"
            , FAnd
                ( FAnd
                    (FImp (propVar "A") (propVar "B"))
                    (FImp (propVar "B") (propVar "C"))
                )
                (propVar "A")
            )
    proofBImpC <- proofAndEProjection ands (FImp (propVar "B") (propVar "C"))
    proofAImpB <- proofAndEProjection ands (FImp (propVar "A") (propVar "B"))
    proofA <- proofAndEProjection ands (propVar "A")
    return
        PImpI
            { hypAntecedent = "h ((A => B) ^ (B => C)) ^ A"
            , -- Dem de C por transitividad
              proofConsequent =
                PImpE
                    { antecedent = propVar "B"
                    , -- Dem de B => C hay que meterse en el AND
                      proofImp = proofBImpC
                    , -- Dem de B
                      proofAntecedent =
                        PImpE
                            { antecedent = propVar "A"
                            , proofImp = proofAImpB
                            , proofAntecedent = proofA
                            }
                    }
            }

-- No tiene tanto sentido usar cut en este caso, porque las implicaciones no se
-- prueban con otras funciones sino que son premisas, así que terminás dando más
-- vueltas con PImpE. Para pasar de A |- B de vuelta a |- A -> B

-- (X ^ Y) v (X ^ Z) => X ^ (Y v Z)
f25 :: Form
f25 =
    FImp
        ( FOr
            (FAnd (propVar "X") (propVar "Y"))
            (FAnd (propVar "X") (propVar "Z"))
        )
        (FAnd (propVar "X") (FOr (propVar "Y") (propVar "Z")))

-- Devuelve una demostración para
-- (X ^ Y) v (X ^ Z) => X ^ (Y v Z)
-- TODO: Mejor nombre
proofDistOrOverAnd :: Form -> Form -> Form -> Proof
proofDistOrOverAnd fx fy fz =
    PImpI
        "h (X ^ Y) v (X ^ Z)"
        -- Queremos demostrar X ^ (Y v Z), necesariamente se repite
        -- o el POrE del antecedente o el PAndI del consecuente
        ( POrE
            { left = FAnd fx fy
            , right = FAnd fx fz
            , proofOr = PAx "h (X ^ Y) v (X ^ Z)"
            , hypLeft = "h X ^ Y"
            , proofAssumingLeft =
                PAndI
                    { proofLeft =
                        PAndE1
                            { right = fy
                            , proofAnd = PAx "h X ^ Y"
                            }
                    , proofRight =
                        POrI1
                            { proofLeft =
                                PAndE2
                                    { left = fx
                                    , proofAnd = PAx "h X ^ Y"
                                    }
                            }
                    }
            , hypRight = "h X ^ Z"
            , proofAssumingRight =
                PAndI
                    { proofLeft =
                        PAndE1
                            { right = fz
                            , proofAnd = PAx "h X ^ Z"
                            }
                    , proofRight =
                        POrI2
                            { proofRight =
                                PAndE2
                                    { left = fx
                                    , proofAnd = PAx "h X ^ Z"
                                    }
                            }
                    }
            }
        )

-- X ^ (Y v Z) => (X ^ Y) v (X ^ Z)
f25' :: Form
f25' =
    FImp
        (FAnd (propVar "X") (FOr (propVar "Y") (propVar "Z")))
        ( FOr
            (FAnd (propVar "X") (propVar "Y"))
            (FAnd (propVar "X") (propVar "Z"))
        )

p25' :: Proof
p25' =
    PImpI
        "h X ^ (Y v Z)"
        -- Dependiendo de si vale Y o Z pruebo el primero o el segundo (respectivamente)
        ( POrE
            { left = propVar "Y"
            , right = propVar "Z"
            , proofOr =
                ( PAndE2
                    { left = propVar "X"
                    , proofAnd = PAx "h X ^ (Y v Z)"
                    }
                )
            , hypLeft = "h Y"
            , proofAssumingLeft =
                POrI1
                    { proofLeft =
                        PAndI
                            { proofLeft =
                                PAndE1
                                    { right = FOr (propVar "Y") (propVar "Z")
                                    , proofAnd = PAx "h X ^ (Y v Z)"
                                    }
                            , proofRight = PAx "h Y"
                            }
                    }
            , hypRight = "h Z"
            , proofAssumingRight =
                POrI2
                    { proofRight =
                        PAndI
                            { proofLeft =
                                PAndE1
                                    { right = FOr (propVar "Y") (propVar "Z")
                                    , proofAnd = PAx "h X ^ (Y v Z)"
                                    }
                            , proofRight = PAx "h Z"
                            }
                    }
            }
        )

{- Dem de prueba con by

thus B by A, A => B
 = (A) ^ (A => B) => B

Queremos demostrar por la negación, viendo que es refutable con dnegelim
Para demostrar que la negación es refutable,
 - eliminamos implicaciones
 - pasamos a forma normal negada (negaciones más adentro posible)
 - la pasamos a DNF con equivalencias
 - refutás cada cláusula encontrando mismos literales sin negar y negados

~ ( ( A ^ (A => B) ) => B )
= ~ (~ (A ^ (A => B)) v B))         [ X => Y = ~X v Y ]
= (~~(A ^ (A => B) ^ ~B))           [ ~(X v Y) = ~X ^ ~Y ]
= A ^ (A => B) ^ ~B                 [ ~~X = X ]
= { A ^ (~A v B) } ^ ~B             [ X => Y = ~X v Y ]
= ((A ^ ~A) v (A ^ B)) ^ ~B         [ X ^ (Y v Z) = (X ^ Y) v (X ^ Z)]
= (A ^ ~A ^ ~B) v (A ^ B ^ ~B)      [ (X v Y) ^ Z = (X ^ Z) v (Y ^ Z)]

en DNF
-}

f26 :: Form
f26 =
    FImp
        ( FAnd
            (propVar "A")
            (FImp (propVar "A") (propVar "B"))
        )
        (propVar "B")

-- TODO arreglar esto que está mal DNF
p26 :: Result Proof
-- Primero doubleNegElim para demostrar por contradicción
p26 = do
    let (dnfNegThesis, dnfProof) = dnf ("h dnfThesis", FNot thesis)
    contradictionProof <- solveContradiction ("h dnfThesis", dnfNegThesis)
    return
        PImpE
            { antecedent = dneg thesis
            , proofImp = doubleNegElim thesis
            , proofAntecedent =
                PNotI
                    { hyp = "h ~((A) ^ (A => B) => B)"
                    , -- Demostración de bottom (contradicción) asumiendo que no vale
                      -- la tesis. Primero convertimos a DNF y luego demostramos que
                      -- la version en DNF es refutable.
                      proofBot =
                        cut
                            dnfNegThesis
                            dnfProof
                            "h dnfThesis"
                            contradictionProof
                    }
            }
  where
    thesis =
        FImp
            ( FAnd
                (propVar "A")
                (FImp (propVar "A") (propVar "B"))
            )
            (propVar "B")

-- (A ^ ~A ^ ~B) v (A ^ B ^ ~B) -> false (bot)
f28_exampleSolve :: Form
f28_exampleSolve =
    FImp
        ( FOr
            ( FAnd
                (propVar "A")
                ( FAnd
                    (FNot $ propVar "A")
                    (FNot $ propVar "B")
                )
            )
            ( FAnd
                (propVar "A")
                ( FAnd
                    (propVar "B")
                    (FNot $ propVar "B")
                )
            )
        )
        FFalse

p28_exampleSolve :: Result Proof
p28_exampleSolve = do
    let left =
            FAnd
                (propVar "A")
                ( FAnd
                    (FNot $ propVar "A")
                    (FNot $ propVar "B")
                )
    let right =
            FAnd
                (propVar "A")
                ( FAnd
                    (propVar "B")
                    (FNot $ propVar "B")
                )
    proofLeftA <- proofAndEProjection ("h (A ^ ~A ^ ~B)", left) (propVar "A")
    proofLeftNotA <- proofAndEProjection ("h (A ^ ~A ^ ~B)", left) (FNot $ propVar "A")
    proofRightB <- proofAndEProjection ("h (A ^ B ^ ~B)", right) (propVar "B")
    proofRightNotB <- proofAndEProjection ("h (A ^ B ^ ~B)", right) (FNot $ propVar "B")
    return
        PImpI
            { hypAntecedent = "h (A ^ ~A ^ ~B) v (A ^ B ^ ~B)"
            , proofConsequent =
                POrE
                    { left = left
                    , right = right
                    , proofOr = PAx "h (A ^ ~A ^ ~B) v (A ^ B ^ ~B)"
                    , hypLeft = "h (A ^ ~A ^ ~B)"
                    , proofAssumingLeft =
                        PNotE
                            { form = propVar "A"
                            , proofNotForm = proofLeftNotA
                            , proofForm = proofLeftA
                            }
                    , hypRight = "h (A ^ B ^ ~B)"
                    , proofAssumingRight =
                        PNotE
                            { form = propVar "B"
                            , proofNotForm = proofRightNotB
                            , proofForm = proofRightB
                            }
                    }
            }

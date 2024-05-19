{-# LANGUAGE QuasiQuotes #-}

module TestCertifier (testCertifier) where

-- https://kseo.github.io/posts/2014-02-06-multi-line-strings-in-haskell.html
-- https://hackage.haskell.org/package/raw-strings-qq
import Text.RawString.QQ

import Certifier (
    certify,
    certifyBy,
    checkContext,
    dnf,
    findContradiction,
    fromClause,
    fromDNF,
    solveContradiction,
    toClause,
 )

import Parser (parseProgram')

import NDProofs (
    EnvItem,
    hypForm,
 )

import NDChecker (
    CheckResult (..),
    check,
 )

import ND (
    Env (..),
    Form (..),
    Proof (..),
    Term (..),
    dneg,
    predVar,
    propVar,
 )

import PPA (Hypothesis (HAxiom))

import Test.HUnit (
    Assertion,
    Counts,
    Test (..),
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
main = do runTestTT testCertifier

testCertifier :: Test
testCertifier =
    test
        [ "certifyBy" ~: testCertifyBy
        , "commands" ~: testCommands
        , "clauses" ~: testClause
        , "findContradiction" ~: testFindContradiction
        , "solve" ~: testSolve
        , "dnf" ~: testDnf
        ]

testProgram :: String -> IO ()
testProgram p = do
    let result = parseProgram' "test" p
    case result of
        Left err -> assertFailure err
        Right prog -> case certify prog of
            Left err -> assertFailure err
            Right ctx -> checkContext ctx @?= Right ()

testProgramError :: String -> String -> IO ()
testProgramError p err = do
    let result = parseProgram' "test" p
    case result of
        Left err -> assertFailure err
        Right prog -> certify prog @?= Left err

testCommands :: Test
testCommands =
    test
        [ "suppose"
            ~: test
                [ "simple"
                    ~: testProgram
                        [r|
                        theorem a_implies_a : a -> a
                        proof
                            suppose "a" : a
                            thus a by a
                        end
                    |]
                , "transitivity"
                    ~: testProgram
                        [r|
                        axiom ax_1 : a -> b
                        axiom ax_2 : b -> c
                        theorem t1 : a -> c 
                        proof
                            suppose a : a
                            // La tesis ahora es c
                            hence c by a, ax_1, ax_2
                        end
                    |]
                , "wrong formula"
                    ~: testProgramError
                        [r|
                            theorem t1: a
                            proof
                                suppose a : a
                            end
                        |]
                        "can't use command 'suppose a : a' with form 'a', must be implication or negation"
                , "not intro"
                    ~: testProgram
                        [r|
                        theorem ej : a -> ~~a
                        proof
                            suppose a : a
                            suppose "no a" : ~a
                            thus false by a, "no a"
                        end
                    |]
                , "not intro contrapositive"
                    ~: testProgram
                        [r|
                        axiom a_implies_b : a -> b
                        theorem contrapositive : ~b -> ~a
                        proof
                            suppose not_b : ~b
                            suppose a : a
                            thus false by a, a_implies_b, not_b
                        end
                    |]
                ]
        , "have"
            ~: testProgram
                [r|
            theorem "ejemplo" : (a -> b -> c) -> (a -> b) -> a -> c
            proof
                suppose "P": a -> b -> c
                suppose "Q": a -> b
                suppose "R": a
                have "S": b by "Q", "R"
                thus c   by "P", "R", "S"
            end
        |]
        , "incomplete proof"
            ~: test
                [ "empty"
                    ~: testProgramError
                        [r|
                        theorem "error": a -> b
                        proof
                        end
                        |]
                        "incomplete proof, still have a -> b as thesis"
                , "incomplete"
                    ~: testProgramError
                        [r|
                        theorem "error": a -> b
                        proof
                            suppose a:a
                        end
                        |]
                        "incomplete proof, still have b as thesis"
                ]
        , "justification not in context error"
            ~: testProgramError
                [r|theorem "ejemplo" : a
                proof
                    thus a by "-", foo
                end|]
                "finding hyps in context: can't get prev hyp from empty ctx; 'foo' not present in ctx"
        , "no contradicting literals error"
            ~: testProgramError
                [r|theorem "ejemplo" : (a -> b -> c) -> (a -> b) -> a -> c
            proof
                suppose "P": a -> b -> c
                suppose "Q": a -> b
                suppose "R": a
                have "S": b by "Q"
            end|]
                "finding contradiction for dnf form '(~a & ~b) | (b & ~b)' obtained from '~((a -> b) -> b)': [~a,~b] contains no contradicting literals or false"
        , "then + hence"
            ~: testProgram
                [r|
            theorem "ejemplo" : (a -> b -> c) -> (a -> b) -> a -> c
            proof
                suppose "P": a -> b -> c
                suppose "Q": a -> b
                suppose "R": a
                then "S": b by "Q"
                hence c   by "P", "R"
            end
            theorem "thus -, have -" : (a -> b -> c) -> (a -> b) -> a -> c
            proof
                suppose "P": a -> b -> c
                suppose "Q": a -> b
                suppose "R": a
                // Sin el sugar, hence == thus -/then == have -
                have "S": b by "Q", -
                thus c   by -, "P", "R"
            end
        |]
        , "and discharge + optional by"
            ~: testProgram
                [r|
            theorem "andi_variant" : a -> b -> (a & b)
            proof
                suppose "a" : a
                suppose "b" : b
                hence b
                thus a by "a"
            end
        |]
        , "tautology + optional by"
            ~: testProgram
                [r|
            theorem "taut" : ~(a | b) -> ~a & ~b
            proof
                // Resuelve solo el solver, sin by
                thus ~(a | b) -> ~a & ~b
            end

            // Demo alternativa pero con have + hence
            theorem "taut con have": ~(a | b) & c -> ~a & ~b & c
            proof
                have "distributiva del not sobre or": ~(a | b) -> ~a & ~b
                hence ~(a | b) & c -> ~a & ~b & c
            end
        |]
        , "and repeteated"
            ~: testProgram
                [r|
            theorem "andi_variant" : a -> b -> (a & (a & (b & a)) & a)
            proof
                suppose "a" : a
                suppose "b" : b
                // A pesar de haber más de un a, se sacan los repetidos
                thus a by "a"
                thus b by "b"
            end
        |]
        , "and discharge complex"
            ~: testProgram
                [r|
            axiom "a": a
            axiom "b": b
            axiom "c": c
            axiom "d": d
            axiom "e": e
            theorem "andi_variant" : (a & b) & ((c & d) & e)
            proof
                thus a & e by "a", "e"
                thus d by "d"
                thus b & c by "b", "c"
            end
        |]
        , -- https://www.cs.ru.nl/~freek/notes/mv.pdf p2
          "freek vernacular"
            ~: testProgram
                [r|
                axiom "a then c": a -> c
                axiom "b then d": b -> d

                theorem t: a & b -> c & d
                proof
                    suppose h : a & b
                    hence c & d by "a then c", "b then d"
                end
            |]
        , "equivalently"
            ~: testProgram
                [r|
                // Reducir la tesis a una fórmula equivalente
                axiom "no a": ~a
                axiom "no b": ~b
                theorem "ejemplo equiv" : ~(a | b)
                proof
                    equivalently (~a & ~b)
                    thus ~a by "no a"
                    thus ~b by "no b"
                end
            |]
        , "claim"
            ~: testProgram
                [r|
                axiom "no a": ~a
                axiom "no b": ~b
                theorem "ejemplo" : ~(a | b)
                proof
                    claim "c" : (~a & ~b)
                    proof
                        thus ~a by "no a"
                        thus ~b by "no b"
                    end
                    thus ~(a | b) by "c"
                end
        |]
        , "cases"
            ~: test
                [ "2 cases"
                    ~: testProgram
                        [r|
                        theorem "ejemplo cases": (a & b) | (c & a) -> a
                        proof
                            suppose h : (a & b) | (c & a)
                            cases by h
                                case a&b
                                    hence a
                                case a&c // no tiene que ser igual
                                    hence a
                            end
                        end
                    |]
                , "3 cases"
                    ~: testProgram
                        [r|
                        axiom d_imp_a : d -> a
                        theorem "3 casos": (a & b) | (c & a) | d -> a
                        proof
                            suppose h : (a & b) | (c & a) | d
                            cases by h
                                case a & b
                                    hence a
                                case c2 : a & c
                                    thus a by c2
                                case d
                                    hence a by d_imp_a
                            end
                        end
                    |]
                ]
        , "take"
            ~: test
                [ "ok"
                    ~: testProgram
                        [r|
                        axiom zero_is_zero : isZero(zero)
                        theorem "zero exists": exists X. isZero(X)
                        proof
                            take X := zero
                            thus isZero(zero) by zero_is_zero
                        end
                |]
                , "error invalid var"
                    ~: testProgramError
                        [r|
                        theorem "zero exists": exists X. isZero(X)
                        proof
                            take Y := zero
                        end
                |]
                        "take: can't take var 'Y', different from thesis var 'X'"
                , "error invalid form"
                    ~: testProgramError
                        [r|
                        theorem "zero exists": a
                        proof
                            take Y := zero
                        end
                |]
                        "take: can't use on form 'a', not exists"
                ]
        , "consider"
            ~: test
                [ "ok"
                    ~: testProgram
                        [r|
                    theorem "consider simple ex": (exists Y . a(Y) & b(Y)) -> exists Q . b(Q)
                    proof
                        suppose h1 : exists Y . a(Y) & b(Y)
                        consider Y st h2 : a(Y) & b(Y) by h1
                        take Q := Y
                        thus b(Y) by -
                    end
                |]
                , "err var free in form"
                    ~: testProgramError
                        [r|
                    theorem "consider": (exists Y . a(Y) & b(Y)) -> exists Q . b(Q)
                    proof
                        suppose h1 : exists Y . a(Y) & b(Y)
                        // Alcanza con invertir estas dos fórmulas para que falle
                        take Q := Y
                        consider Y st h2 : a(Y) & b(Y) by h1
                        thus b(Y) by -
                    end
                |]
                        "consider: can't use an exist whose variable (Y) appears free in the thesis (b(Y))"
                , "err var free in ctx"
                    ~: testProgramError
                        [r|
                    axiom a1 : exists Y . a(Y)
                    theorem "consider": (exists Y . b(Y)) -> exists Y . c(Y)
                    proof
                        suppose h1 : exists Y . b(Y)
                        consider Y st h2 : a(Y) by a1
                        consider Y st h3 : b(Y) by h1 // está libre en el contexto
                    end
                |]
                        "consider: can't use an exist whose variable (Y) appears free in the preceding context ([(axiom) h2 : a(Y),(axiom) h1 : exists Y . b(Y),(axiom) a1 : exists Y . a(Y)])"
                ]
        , "let"
            ~: [ "err var in context"
                    ~: testProgramError
                        [r|
                    axiom a1: exists X . a(X)
                    theorem "let" : forall X . a(X)
                    proof
                        consider X st h : a(X) by a1
                        let X := X // está X libre en el ctx por el consider
                        thus a(X) by a1
                    end
                |]
                        "let: new var (X) must not appear free in preceding context ([(axiom) h : a(X),(axiom) a1 : exists X . a(X)])"
               , "wrong form"
                    ~: testProgramError
                        [r|
                    theorem "let err" : a -> b
                    proof
                        let X := X
                    end
                |]
                        "let: can't use with form 'a -> b', must be an universal quantifier (forall)"
               ]
        ]

-- , "optional hyp"
--     ~: testProgram
--         [r|
--     theorem "ejemplo sin hyp id" : (a -> b -> c) -> (a -> b) -> a -> c
--     proof
--         suppose "P": a -> b -> c
--         suppose "Q": a -> b
--         suppose "R": a
--         then b by "Q" // no tiene hyp id
--         hence c by "P", "R"
--     end|]

testSolve :: Test
testSolve =
    test
        [ "refutable single clause w/ false"
            ~: doTestSolveEqCheck
                ("h", fromClause [FFalse, propVar "Q"])
                ( PNamed
                    "contradiction of false & Q by false"
                    PAndE1{right = propVar "Q", proofAnd = PAx "h"}
                )
        , "refutable single clause w/ opposites"
            ~: doTestSolveEqCheck
                ("h", fromClause [propVar "A", FNot $ propVar "A"])
                ( PNamed
                    "contradiction of A & ~A by A and ~A"
                    PNotE
                        { form = propVar "A"
                        , proofNotForm =
                            PAndE2
                                { left = propVar "A"
                                , proofAnd = PAx "h"
                                }
                        , proofForm =
                            PAndE1
                                { right = FNot $ propVar "A"
                                , proofAnd = PAx "h"
                                }
                        }
                )
        , "refutable dnf three clauses"
            ~: doTestSolveEqCheck
                ( "h"
                , fromDNF
                    [ [propVar "A", FNot $ propVar "A"]
                    , [FFalse, propVar "Q"]
                    , [FNot $ propVar "B", propVar "B"]
                    ]
                )
                POrE
                    { left =
                        fromDNF
                            [ [propVar "A", FNot $ propVar "A"]
                            , [FFalse, propVar "Q"]
                            ]
                    , right = fromClause [FNot $ propVar "B", propVar "B"]
                    , proofOr = PAx "h"
                    , hypLeft = "h L"
                    , proofAssumingLeft =
                        POrE
                            { left = fromClause [propVar "A", FNot $ propVar "A"]
                            , right = fromClause [FFalse, propVar "Q"]
                            , proofOr = PAx "h L"
                            , hypLeft = "h L L"
                            , proofAssumingLeft =
                                PNamed
                                    "contradiction of A & ~A by A and ~A"
                                    PNotE
                                        { form = propVar "A"
                                        , proofNotForm =
                                            PAndE2
                                                { left = propVar "A"
                                                , proofAnd = PAx "h L L"
                                                }
                                        , proofForm =
                                            PAndE1
                                                { right = FNot $ propVar "A"
                                                , proofAnd = PAx "h L L"
                                                }
                                        }
                            , hypRight = "h L R"
                            , proofAssumingRight =
                                PNamed
                                    "contradiction of false & Q by false"
                                    PAndE1
                                        { right = propVar "Q"
                                        , proofAnd = PAx "h L R"
                                        }
                            }
                    , hypRight = "h R"
                    , proofAssumingRight =
                        PNamed
                            "contradiction of ~B & B by B and ~B"
                            PNotE
                                { form = propVar "B"
                                , proofNotForm =
                                    PAndE1
                                        { right = propVar "B"
                                        , proofAnd = PAx "h R"
                                        }
                                , proofForm =
                                    PAndE2
                                        { left = FNot $ propVar "B"
                                        , proofAnd = PAx "h R"
                                        }
                                }
                    }
        , "refutable long"
            ~: doTestSolveCheck
            $ fromDNF
                [
                    [ FExists "x" (predVar "P" "x")
                    , FTrue
                    , propVar "Q"
                    , FNot $ FExists "x" (predVar "P" "x")
                    ]
                , [propVar "X", FFalse, FNot $ propVar "Q"]
                , [propVar "A", propVar "B", FNot $ propVar "A", FNot $ propVar "B"]
                ,
                    [ FForall "y" (predVar "Q" "y")
                    , FTrue
                    , propVar "Q"
                    , FNot $ FForall "y" (predVar "Q" "y")
                    ]
                ]
        , "clause too short not refutable"
            ~: solveContradiction ("h", fromDNF [[propVar "X"]])
            ~?= Left "[X] contains no contradicting literals or false"
        , "one clause not refutable"
            ~: solveContradiction
                ( "h"
                , fromDNF
                    [
                        [ FExists "x" (predVar "P" "x")
                        , FTrue
                        , propVar "Q"
                        , FNot $ FExists "x" (predVar "P" "x")
                        ]
                    , [propVar "X", FFalse, FNot $ propVar "Q"]
                    , [propVar "A", propVar "B", FNot $ propVar "A", FNot $ propVar "B"]
                    , [propVar "X", FTrue]
                    ]
                )
            ~?= Left "[X,true] contains no contradicting literals or false"
        , "not dnf" ~: solveContradiction ("h", FImp FTrue FFalse) ~?= Left "convert to clause: true -> false is not a literal"
        ]

doTestSolveEqCheck :: EnvItem -> Proof -> IO ()
doTestSolveEqCheck i@(h, f) expectedProof = do
    let result = solveContradiction i
    result @?= Right expectedProof
    let (Right proof) = result
    check (EExtend h f EEmpty) proof FFalse @?= CheckOK

doTestSolveCheck :: Form -> Assertion
doTestSolveCheck f = do
    let result = solveContradiction ("h", f)
    case result of
        Right proof -> check (EExtend "h" f EEmpty) proof FFalse @?= CheckOK
        Left err -> assertFailure err

testCertifyBy :: Test
testCertifyBy =
    test
        [ "A by A" ~: do
            let ctx = [HAxiom "A" a]
            let env = EExtend "A" a EEmpty
            let result = certifyBy ctx a ["A"]
            case result of
                Left e -> assertFailure e
                Right proof -> CheckOK @=? check env proof a
        , "A by A & B" ~: do
            let ctx = [HAxiom "A and B" (FAnd a b)]
            let env = EExtend "A and B" (FAnd a b) EEmpty
            let result = certifyBy ctx a ["A and B"]
            case result of
                Left e -> assertFailure e
                Right proof -> CheckOK @=? check env proof a
        , "A by (A & B) v (A & C)" ~: do
            let ax = FOr (FAnd a b) (FAnd a c)
            let ctx = [HAxiom "ax" ax]
            let env = EExtend "ax" ax EEmpty
            let result = certifyBy ctx a ["ax"]
            case result of
                Left e -> assertFailure e
                Right proof -> CheckOK @=? check env proof a
        , "B by A, A => B" ~: do
            let ctx =
                    [ HAxiom "A is always true" a
                    , HAxiom "A implies B" (FImp a b)
                    ]
            let env = EExtend "A is always true" a (EExtend "A implies B" (FImp a b) EEmpty)
            let result = certifyBy ctx b ["A is always true", "A implies B"]
            case result of
                Left e -> assertFailure e
                Right proof -> do
                    CheckOK @=? check env proof b
        ]
  where
    (a, b, c) = (propVar "A", propVar "B", propVar "C")

testFindContradiction :: Test
testFindContradiction =
    test
        [ "false contradiction"
            ~: findContradiction
                [ propVar "A"
                , propVar "B"
                , FFalse
                ]
            ~?= Right
                FFalse
        , "no contradiction"
            ~: findContradiction
                [ propVar "A"
                , propVar "B"
                , propVar "C"
                ]
            ~?= Left "[A,B,C] contains no contradicting literals or false"
        , "literals contradicting"
            ~: findContradiction
                [ propVar "A"
                , propVar "B"
                , FNot $ propVar "A"
                , propVar "C"
                ]
            ~?= Right (propVar "A")
        , "more than one literal contradicting returns first"
            ~: findContradiction
                [ FForall "x" (propVar "A")
                , propVar "B"
                , FNot $ propVar "B"
                , propVar "C"
                , FNot $ FForall "x" (propVar "A")
                ]
            ~?= Right (FForall "x" (propVar "A"))
        ]

testClause :: Test
testClause =
    test
        [ "not clause error"
            ~: toClause (FOr (propVar "A") (propVar "B"))
            ~?= Left
                "convert to clause: A | B is not a literal"
        , "not literals error"
            ~: toClause (FNot FTrue)
            ~?= Left "convert to clause: ~true is not a literal"
        , "full clause"
            ~: toClause
                ( FAnd
                    (FNot $ propVar "A")
                    ( FAnd
                        (FExists "x" (predVar "A" "x"))
                        ( FAnd
                            (FForall "x" (predVar "B" "x"))
                            (FAnd FTrue FFalse)
                        )
                    )
                )
            ~?= Right
                [ FNot $ propVar "A"
                , FExists
                    "x"
                    (predVar "A" "x")
                , FForall
                    "x"
                    (predVar "B" "x")
                , FTrue
                , FFalse
                ]
        , "fromClause"
            ~: fromClause [propVar "A", propVar "B", propVar "C", propVar "D"]
            ~?= FAnd
                ( FAnd
                    ( FAnd
                        (propVar "A")
                        (propVar "B")
                    )
                    (propVar "C")
                )
                (propVar "D")
        , "fromDNF"
            ~: fromDNF
                [ [propVar "A", propVar "B", propVar "Q"]
                , [propVar "C", FNot $ propVar "A"]
                , [propVar "X"]
                ]
            ~?= FOr
                ( FOr
                    ( FAnd
                        ( FAnd
                            (propVar "A")
                            (propVar "B")
                        )
                        (propVar "Q")
                    )
                    (FAnd (propVar "C") (FNot $ propVar "A"))
                )
                (propVar "X")
        ]

testDnf :: Test
testDnf =
    test
        [ "imp elim: x => y / ~x v y"
            ~: doTestDNF
                (FImp x y)
                (FOr (FNot x) y)
        , "dneg elim: ~~x / x" ~: doTestDNF (dneg x) x
        , "not dist over and: ~(x ^ y) / ~x v ~y"
            ~: doTestDNF
                (FNot (FAnd x y))
                (FOr (FNot x) (FNot y))
        , "and assoc: x ^ ((y ^ z) ^ (p ^ q)) / (((x ^ y) ^ z) ^ p) ^ q"
            ~: doTestDNF
                (FAnd x (FAnd (FAnd y z) (FAnd p q)))
                (FAnd (FAnd (FAnd (FAnd x y) z) p) q)
        , "or assoc: x v ((y v z) v (p v q)) / (((x v y) v z) v p) v q"
            ~: doTestDNF
                (FOr x (FOr (FOr y z) (FOr p q)))
                (FOr (FOr (FOr (FOr x y) z) p) q)
        , "not dist over or: ~(x v y) / ~x ^ ~y"
            ~: doTestDNF
                (FNot (FOr x y))
                (FAnd (FNot x) (FNot y))
        , "imp elim + or cong2 + not dist over and: x => ~(y ^ z) / ~x v ~y v ~z"
            ~: doTestDNF
                (FImp x (FNot $ FAnd y z))
                (FOr (FOr (FNot x) (FNot y)) (FNot z))
        , "not cong + imp elim + dnegelim: ~(x => y) / x ^ ~y"
            ~: doTestDNF
                (FNot $ FImp x y)
                (FAnd x (FNot y))
        , "not true y not false"
            ~: doTestDNF
                (FAnd (FNot FTrue) (FNot FFalse))
                (FAnd FFalse FTrue)
        , "and dist over or L"
            ~: doTestDNF
                (FAnd x (FOr y z))
                (FOr (FAnd x y) (FAnd x z))
        , "and dist over or R"
            ~: doTestDNF
                (FAnd (FOr y z) x)
                (FOr (FAnd y x) (FAnd z x))
        , "~ ((x ^ (x => y)) => y) / (x ^ ~x ^ ~y) v (x ^ y ^ ~y)"
            ~: doTestDNF
                (FNot (FImp (FAnd x (FImp x y)) y))
                ( fromDNF
                    [[x, FNot x, FNot y], [x, y, FNot y]]
                )
        , "exists and forall opaque: ~ [ (E x. (P => Q)) ^ (Y v Z) ]"
            ~: do
                let exists = FExists "x" (FImp p q)
                let fall = FForall "y" (FImp p q)
                doTestDNF
                    ( FNot (FAnd exists (FAnd fall (FOr y z)))
                    )
                    ( fromDNF
                        [ [FNot exists]
                        , [FNot fall]
                        , [FNot y, FNot z]
                        ]
                    )
        ]
  where
    p = propVar "p"
    q = propVar "q"
    x = propVar "x"
    y = propVar "y"
    z = propVar "z"

doTestDNF :: Form -> Form -> Assertion
doTestDNF f fDNF = do
    let hF = hypForm f
    let (fGotDNF, dnfProof) = dnf (hF, f)
    fGotDNF @?= fDNF
    CheckOK @=? check (EExtend hF f EEmpty) dnfProof fDNF
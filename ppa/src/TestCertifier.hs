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
    partitionForalls,
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
    fPred1,
    predVar,
    propVar,
    tFun0,
    tFun1,
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
        , "programs" ~: testPrograms
        , "clauses" ~: testClause
        , "findContradiction" ~: testFindContradiction
        , "solve" ~: testSolve
        , "dnf" ~: testDnf
        , "partitionForalls" ~: testPartitionForalls
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

testPrograms :: Test
testPrograms =
    test
        [ "relatives old"
            ~: testProgram
                [r|
axiom todos_tienen_padre: forall P. exists Q. padre(P, Q)
axiom def_abuelo:
    forall P. forall Q. forall R.
        (padre(P, Q) & padre(Q, R)) -> abuelo(P, R)

theorem todos_tienen_abuelo: forall A. exists B. abuelo(A, B)
proof
    // Primero encuentro el padre del padre de A (todos tienen padre)
    // Luego uso la definición para mostrar que es el abuelo
    let A
    have -: exists B. padre(A, B) by todos_tienen_padre
    consider B st a_padre_b: padre(A, B) by -

    have -: exists C. padre(B, C) by todos_tienen_padre
    consider C st b_padre_c: padre(B, C) by -

    take B := C
    
    // me gustaria
    //thus abuelo(A, C) by a_padre_b, b_padre_c, def_abuelo

    // Tengo que hacer la eliminación en 3 pasos porque se elimina de a un forall
    have -: forall B. forall C. padre(A, B) & padre(B, C) -> abuelo(A, C)
        by def_abuelo
    then -: forall C. padre(A, B) & padre(B, C) -> abuelo(A, C)
    then -: padre(A, B) & padre(B, C) -> abuelo(A, C)
    hence abuelo(A, C) by a_padre_b, b_padre_c
end
    |]
        , "relatives new"
            ~: testProgram
                [r|
    axiom todos_tienen_padre: forall P. exists Q. padre(P, Q)
axiom def_abuelo:
    forall P. forall Q. forall R.
        (padre(P, Q) & padre(Q, R)) -> abuelo(P, R)

axiom def_hijo: forall P. forall Q. padre(P, Q) -> hijo(Q, P)

theorem todos_tienen_abuelo: forall A. exists B. abuelo(A, B)
proof
    // Primero encuentro el padre del padre de A (todos tienen padre)
    // Luego uso la definición para mostrar que es el abuelo
    let A
    consider B st a_padre_b: padre(A, B) by todos_tienen_padre
    consider C st b_padre_c: padre(B, C) by todos_tienen_padre

    take B := C

    thus abuelo(A, C) by a_padre_b, b_padre_c, def_abuelo
end|]
        , "groups"
            ~: testProgram
                [r|
    // Teoría matemática de Grupos //
    //
    // Referencia: https://en.wikipedia.org/wiki/Group_(mathematics)#Uniqueness_of_identity_element
    // Simbolos
    // - op/2: El operador * del grupo
    // - eq/2: Igualdad
    // - id/1: Predicado que indica que un elemento es la identidad

    axiom eq_refl: forall X . eq(X, X)
    axiom eq_sym: forall X. forall Y. eq(X, Y) -> eq(Y, X)
    axiom eq_trans: forall X. forall Y. forall Z.
        eq(X, Y) & eq(Y, Z) -> eq(X, Z)

    axiom id_def: forall E. forall X.
        (eq(op(E, X), X) & eq(op(X, E), X) -> id(E)) &
        (id(E) -> eq(op(E, X), X) & eq(op(X, E), X))


    axiom op_cong_eq_1: forall X. forall Y. forall Z.
        eq(X, Y) -> eq(op(X, Z), op(Y, Z))

    axiom op_cong_eq_2: forall X. forall Y. forall Z.
        eq(Y, Z) -> eq(op(X, Y), op(X, Z))

    axiom id_exists: exists E. id(E)

    axiom inverse_def: forall E . forall A . forall I .
        id(E) -> (
            (inverse(A, I) -> eq(op(A, I), E) & eq(op(I, A), E)) &
            (eq(op(A, I), E) & eq(op(I, A), E) -> inverse(A, I) )
        )

    axiom inverse_exists: forall A . exists I . inverse(A, I)

    // (X * Y) * Z = X * (Y * Z)
    axiom assoc_def: forall X. forall Y. forall Z.
        eq(
            op(op(X, Y), Z),
            op(X, op(Y, Z))
        )

    /* sketch
        Si e1 y e2 son identity elements,

        por e1 usando e2
            (1) e2 * e1 = e1 * e2 = e2
        
        por e2 usando e1
            (2) e1 * e2 = e2 * e1 = e1
        
        luego tengo
            e1 * e2 = e2 y
            e1 * e2 = e1
        
        luego,

            e1 = e1 * e2 (eq_sym)
            = e2
            -> e1 = e2 (eq_trans)
    */
    theorem identity_unique_2:
        forall E1. forall E2.
            id(E1) & id(E2) -> eq(E1, E2)
    proof
        let E1
        let E2
        suppose e1_e2_are_id: id(E1) & id(E2)

        // qvq eq(E1, E2)
        have "e2 * e1 = e2": eq(op(E2, E1), E2)
            by e1_e2_are_id, id_def

        have "e2 * e1 = e1": eq(op(E2, E1), E1)
            by e1_e2_are_id, id_def

        have "e1 = e2 * e1": eq(E1, op(E2, E1)) by "e2 * e1 = e1", eq_sym

        thus eq(E1, E2) by "e1 = e2 * e1", "e2 * e1 = e2", eq_trans
    end


    // b = b * e            (e es la identidad)
    //   = b * (a * c)      (c es inverso de a)
    //   = (b * a) * c      (assoc)
    //   = e * c            (b inverso de a)
    //   = c                (e es id)
    theorem inverse_unique:
        forall A . forall B . forall C .
            inverse(A, B) & inverse(A, C) -> eq(B, C)
    proof
        let A
        let B
        let C
        suppose b_c_inverse_of_a: inverse(A, B) & inverse(A, C)

        consider E st id_e:id(E) by id_exists

        have "b*e = b": eq(op(B, E), B) by id_def, id_e

        have "b = b*e": eq(B, op(B, E)) by "b*e = b", eq_sym

        have "a * c = e": eq(op(A, C), E) by inverse_def, b_c_inverse_of_a, id_e
        
        have "e = a*c": eq(E, op(A, C)) by "a * c = e", eq_sym

        have "b * e = b * (a * c)": eq(op(B, E), op(B, op(A, C)))
            by "e = a*c", op_cong_eq_2

        have "(b * a) * c = b * (a * c)": eq(op(op(B, A), C), op(B, op(A, C)))
            by assoc_def
        then "b * (a * c) = (b * a) * c": eq(op(B, op(A, C)), op(op(B, A), C))
            by eq_sym
        
        have "b * a = e": eq(op(B, A), E) by b_c_inverse_of_a, inverse_def, id_e
        have "(b * a) * c = e * c": eq(op(op(B, A), C), op(E, C))
            by "b * a = e", op_cong_eq_1

        have "e * c = c": eq(op(E, C), C) by id_def, id_e

        // pasos de transitividad
        have "b = b * (a * c)": eq(B, op(B, op(A, C)))
            by "b = b*e", "b * e = b * (a * c)", eq_trans
        
        then "b = (b * a) * c": eq(B, op(op(B, A), C))
            by "b * (a * c) = (b * a) * c", eq_trans
        
        then "b = e * c": eq(B, op(E, C))
            by "(b * a) * c = e * c", eq_trans
        
        hence eq(B, C)
            by "e * c = c", eq_trans
    end
    |]
        ]

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
                        "theorem 't1': \n certify suppose: can't suppose 'a : a' with form 'a', must be implication or negation"
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
        , "invalid axiom free vars"
            ~: testProgramError
                [r|
            axiom a: p(X) & g(Y)
        |]
                "axiom 'a': can't have free vars but have {X,Y}"
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
                        "theorem 'error': incomplete proof, still have a -> b as thesis"
                , "incomplete"
                    ~: testProgramError
                        [r|
                        theorem "error": a -> b
                        proof
                            suppose a:a
                        end
                        |]
                        "theorem 'error': \n certify suppose: incomplete proof, still have b as thesis"
                ]
        , "justification not in context error"
            ~: testProgramError
                [r|theorem "ejemplo" : a
                proof
                    thus a by "-", foo
                end|]
                "theorem 'ejemplo': \n certify thus: finding hyps in context: can't get prev hyp from empty ctx; 'foo' not present in ctx"
        , "no contradicting literals error"
            ~: testProgramError
                [r|theorem "ejemplo" : (a -> b -> c) -> (a -> b) -> a -> c
            proof
                suppose "P": a -> b -> c
                suppose "Q": a -> b
                suppose "R": a
                have "S": b by "Q"
            end|]
                "theorem 'ejemplo': \n certify suppose: \n certify suppose: \n certify suppose: \n certify have (S): solving form by finding contradiction of negation:\n'~((a -> b) -> b)',\nin dnf: '(~a & ~b) | (b & ~b)': '~a & ~b' contains no contradicting literals or false, and trying to eliminate foralls: no foralls to eliminate"
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
        , "LEM"
            ~: testProgram
                [r|
                theorem lem: p | ~p
                proof
                    thus p | ~p
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
                , "optional by"
                    ~: testProgram
                        [r|
                    axiom a1: p -> q
                    axiom a2: ~p -> q

                    theorem always_q: q
                    proof
                        cases
                        case p
                            thus q by a1, -
                        case ~p
                            thus q by a2, -
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
                        "theorem 'zero exists': \n certify take: can't take var 'Y', different from thesis var 'X'"
                , "error invalid form"
                    ~: testProgramError
                        [r|
                        theorem "zero exists": a
                        proof
                            take Y := zero
                        end
                |]
                        "theorem 'zero exists': \n certify take: can't use on form 'a', not exists"
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
                , "ok rename"
                    ~: testProgram
                        [r|
                    theorem "consider simple ex": (exists Y . a(Y) & b(Y)) -> exists Q . b(Q)
                    proof
                        suppose h1 : exists Y . a(Y) & b(Y)
                        consider Q st h2 : a(Q) & b(Q) by h1
                        take Q := Q
                        thus b(Q) by -
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
                        "theorem 'consider': \n certify suppose: \n certify take: \n certify consider: can't use an exist whose variable (Y) appears free in the thesis (b(Y))"
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
                        "theorem 'consider': \n certify suppose: \n certify consider: \n certify consider: can't use an exist whose variable (Y) appears free in the preceding context ([(axiom) h2 : a(Y),(axiom) h1 : exists Y . b(Y),(axiom) a1 : exists Y . a(Y)])"
                ]
        , "let"
            ~: [ "err var in context"
                    ~: testProgramError
                        [r|
                    axiom a1: exists X . a(X)
                    theorem "let" : forall X . a(X)
                    proof
                        consider X st h : a(X) by a1
                        let X // está X libre en el ctx por el consider
                        thus a(X) by a1
                    end
                |]
                        "theorem 'let': \n certify consider: \n certify let: new var (X) must not appear free in preceding context ([(axiom) h : a(X),(axiom) a1 : exists X . a(X)])"
               , "wrong form"
                    ~: testProgramError
                        [r|
                    theorem "let err" : a -> b
                    proof
                        let X
                    end
                |]
                        "theorem 'let err': \n certify let: can't use with form 'a -> b', must be an universal quantifier (forall)"
               , "var free in thesis"
                    ~: testProgramError
                        [r|
                    axiom a: forall X. p(X, X)
                    theorem t: forall X. forall Y. p(X, Y)
                    proof
                        let X // forall Y . p(X, Y)
                        let X // p(X,X), pero X está libre en la tesis, falla
                        thus p(X, X) by a
                    end
                |]
                        "theorem 't': \n certify let: \n certify let: new var (X) must not appear free in forall: forall Y . p(X, Y)"
               , "ok"
                    ~: testProgram
                        [r|
                    axiom a1: forall X . p(X) & q(X)
                    theorem "let" : forall X . p(X)
                    proof
                        let X
                        thus p(X) by a1
                    end

                    theorem "let rename" : forall X . p(X)
                    proof
                        let Y
                        thus p(Y) by a1
                    end
               |]
               ]
        , "forall elim"
            ~: test
                [ "simple ex OK"
                    ~: testProgram
                        [r|
                    axiom a: forall X. f(X)
                    theorem t: f(a)
                    proof
                        thus f(a) by a
                    end
                |]
                , "ok more than one"
                    ~: testProgram
                        [r|
                    axiom a0: forall Y. g(Y)
                    axiom a: forall X. f(X)
                    theorem t: f(a)
                    proof
                        thus f(a) by a0, a
                    end
                |]
                , "dnf OK"
                    ~: testProgram
                        [r|
                    // Al instanciar la X, queda una fórmula que no está en DNF
                    //  ~(f(X) | g(X)) & ~f(a) & ~g(a)
                    // y hay que re-convertir

                    axiom a: forall X. ~(f(X) | g(X))
                    theorem t: ~f(a) & ~g(a)
                    proof
                        thus ~f(a) & ~g(a) by a
                    end
                |]
                , "with imp"
                    ~: testProgram
                        [r|
                    axiom a: forall X. f(X) | g(X)
                    theorem t: ~g(a) -> f(a)
                    proof
                        suppose h: ~g(a)
                        thus f(a) by -, a
                    end
                |]
                , "no vars inside forall"
                    ~: testProgram
                        [r|
                    axiom a: forall X. f(a)
                    theorem t: f(a)
                    proof
                        thus f(a) by a
                    end

                    axiom a2: forall X. false
                    theorem t2: f(b)
                    proof
                        thus f(b) by a2
                    end
                |]
                , "ok alpha equiv"
                    ~: testProgram
                        [r|
                    axiom a1: forall X. forall Y. f(X) & g(Y)
                    theorem t: forall Z. f(a) & g(Z)
                    proof
                        thus forall Z. f(a) & g(Z) by a1
                    end
                |]
                , "elim more than one in same clause OK"
                    ~: testProgram
                        [r|
                        axiom a1: forall X . forall Y. p(X, Y)
                        theorem t: p(a, b)
                        proof
                            thus p(a, b) by a1
                        end

                        // Tambien se puede hacer por pasos
                        theorem t2: p(a, b)
                        proof
                            have -: forall Y. p(a, Y) by a1
                            hence p(a, b)
                        end
                        |]
                , "elim more than one in different clause OK"
                    ~: testProgram
                        [r|
                        axiom a1: forall Y. q(Y)
                        axiom a2: forall X. p(X)
                        theorem t: p(a) & q(b)
                        proof
                            /* elimina los 2 foralls porque termina en cláusulas diferentes
                            ~(a1 & a2 -> p(a) & p(b))
                            a1 & a2 & ~(p(a) & p(b))
                            a1 & a2 & (~p(a) | ~p(b))
                            ~p(a) & a1 & a2 | ~p(b) & a1 & a2
                            */
                            thus p(a) & q(b) by a1, a2
                        end
                        |]
                , "elim more than one forall for same clause error"
                    ~: testProgramError
                        [r|
                        axiom a1: forall Y. q(Y) -> p(Y)
                        axiom a2: forall X. q(X)
                        theorem t: p(a)
                        proof
                            thus p(a) by a1, a2
                        end
                        |]
                        "theorem 't': \n certify thus: solving form by finding contradiction of negation:\n'~((forall Y . (q(Y) -> p(Y)) & forall X . q(X)) -> p(a))',\nin dnf: '(forall Y . (q(Y) -> p(Y)) & forall X . q(X)) & ~p(a)': '(forall Y . (q(Y) -> p(Y)) & forall X . q(X)) & ~p(a)' contains no contradicting literals or false, and trying to eliminate foralls: no foralls useful for contradictions:\ntry eliminating 'forall X . q(X)': solving clause with 'X' replaced by metavar '?0' and reconverting to dnf \n[forall Y . (q(Y) -> p(Y)),q(?0),~p(a)]\n(subst {}) no opposites that unify in clause [forall Y . (q(Y) -> p(Y)),q(?0),~p(a)]\nno more foralls\ntry eliminating 'forall Y . (q(Y) -> p(Y))': solving clause with 'Y' replaced by metavar '?0' and reconverting to dnf \n[~q(?0),forall X . q(X),~p(a)]\n[p(?0),forall X . q(X),~p(a)]\n(subst {}) no opposites that unify in clause [~q(?0),forall X . q(X),~p(a)]\nno more foralls"
                , "elim more than one forall for same clause multiple steps OK"
                    ~: testProgram
                        [r|
                        axiom a1: forall Y. q(Y) -> p(Y)
                        axiom a2: forall X. q(X)
                        theorem t: p(a)
                        proof
                            have -: q(a) -> p(a) by a1
                            thus p(a) by a2, -
                        end
                        |]
                , "err no foralls"
                    ~: testProgramError
                        [r|
                    axiom a1: forall X. f(X)
                    axiom a2: forall Y. g(Y)
                    theorem t: h(a)
                    proof
                        thus h(a) by a1, a2
                    end
                |]
                        "theorem 't': \n certify thus: solving form by finding contradiction of negation:\n'~((forall X . f(X) & forall Y . g(Y)) -> h(a))',\nin dnf: '(forall X . f(X) & forall Y . g(Y)) & ~h(a)': '(forall X . f(X) & forall Y . g(Y)) & ~h(a)' contains no contradicting literals or false, and trying to eliminate foralls: no foralls useful for contradictions:\ntry eliminating 'forall Y . g(Y)': solving clause with 'Y' replaced by metavar '?0' and reconverting to dnf \n[forall X . f(X),g(?0),~h(a)]\n(subst {}) no opposites that unify in clause [forall X . f(X),g(?0),~h(a)]\nno more foralls\ntry eliminating 'forall X . f(X)': solving clause with 'X' replaced by metavar '?0' and reconverting to dnf \n[f(?0),forall Y . g(Y),~h(a)]\n(subst {}) no opposites that unify in clause [f(?0),forall Y . g(Y),~h(a)]\nno more foralls"
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
            ~?= Left "'X' contains no contradicting literals or false, and trying to eliminate foralls: no foralls to eliminate"
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
            ~?= Left "'X & true' contains no contradicting literals or false, and trying to eliminate foralls: no foralls to eliminate"
        , "not dnf" ~: solveContradiction ("h", FImp FTrue FFalse) ~?= Left "convert to clause: true -> false is not a literal"
        , "ok forall 1 form"
            ~: doTestSolveCheck
            $ fromClause [FForall "X" $ fPred1 "p" (TVar "X"), FNot $ fPred1 "p" (tFun0 "a")]
        , "ok forall 2 forms"
            ~: doTestSolveCheck
            $ fromClause
                [ FForall
                    "X"
                    ( fromDNF
                        [
                            [ predVar "p" "X"
                            , FNot $ fPred1 "p" (tFun0 "a")
                            , FNot $ fPred1 "p" (tFun0 "b")
                            ]
                        ,
                            [ predVar "p" "X"
                            , FNot $ fPred1 "p" (tFun0 "a")
                            ]
                        ]
                    )
                ]
        , "ok forall 2 forms match second"
            -- two valid substs, must use second
            ~: doTestSolveCheck
            $ fromClause
                [ FForall
                    "X"
                    ( fromDNF
                        [
                            [ predVar "p" "X"
                            , FNot $ fPred1 "p" (tFun0 "a")
                            , FNot $ fPred1 "p" (tFun0 "b")
                            ]
                        ,
                            [ predVar "p" "X"
                            , FNot $ fPred1 "p" (tFun0 "b")
                            ]
                        ]
                    )
                ]
        , "err forall 2 forms no match second"
            ~: solveContradiction
                ( "h"
                , fromClause
                    [ FForall
                        "X"
                        ( fromDNF
                            [
                                [ predVar "p" "X"
                                , FNot $ fPred1 "p" (tFun0 "a")
                                , FNot $ fPred1 "p" (tFun0 "b")
                                ]
                            ,
                                [ predVar "p" "X"
                                , FNot $ fPred1 "p" (tFun0 "c")
                                ]
                            ]
                        )
                    ]
                )
            ~?= Left "'forall X . (((p(X) & ~p(a)) & ~p(b)) | (p(X) & ~p(c)))' contains no contradicting literals or false, and trying to eliminate foralls: no foralls useful for contradictions:\ntry eliminating 'forall X . (((p(X) & ~p(a)) & ~p(b)) | (p(X) & ~p(c)))': solving clause with 'X' replaced by metavar '?0' and reconverting to dnf \n[p(?0),~p(a),~p(b)]\n[p(?0),~p(c)]\n(subst {0=a}) no opposites that unify in clause [p(?0),~p(c)]\n(subst {0=b}) no opposites that unify in clause [p(?0),~p(c)]\nno more foralls"
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
            ~?= Just
                FFalse
        , "no contradiction"
            ~: findContradiction
                [ propVar "A"
                , propVar "B"
                , propVar "C"
                ]
            ~?= Nothing
        , "literals contradicting"
            ~: findContradiction
                [ propVar "A"
                , propVar "B"
                , FNot $ propVar "A"
                , propVar "C"
                ]
            ~?= Just (propVar "A")
        , "more than one literal contradicting returns first"
            ~: findContradiction
                [ FForall "x" (propVar "A")
                , propVar "B"
                , FNot $ propVar "B"
                , propVar "C"
                , FNot $ FForall "x" (propVar "A")
                ]
            ~?= Just (FForall "x" (propVar "A"))
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

testPartitionForalls :: Test
testPartitionForalls =
    test
        [ partitionForalls [fx, pa, fy, fz, qa]
            ~?= [ ([fx, pa, fy], fz, [qa])
                , ([fx, pa], fy, [fz, qa])
                , ([], fx, [pa, fy, fz, qa])
                ]
        , partitionForalls [fx, fy, fz]
            ~?= [ ([fx, fy], fz, [])
                , ([fx], fy, [fz])
                , ([], fx, [fy, fz])
                ]
        ]
  where
    fx = FForall "X" (predVar "p" "X")
    fy = FForall "Y" (predVar "p" "Y")
    fz = FForall "Z" (predVar "p" "Z")
    pa = fPred1 "p" (tFun0 "a")
    qa = fPred1 "q" (tFun0 "b")

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
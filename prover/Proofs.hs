module Proofs where

import Prover
    ( Proof(PAx, PImpE, PLEM, PFalseE, PImpI, PNotI, POrE, PAndE1,
            PNotE, PAndE2, PAndI, POrI2, POrI1, PTrueI, PExistsI, PExistsE, PForallI, PForallE),
      Form(FNot, FPred, FFalse, FImp, FOr, FAnd, FTrue, FForall, FExists),
      PredId, Term (TVar), VarId )

-- Dems sacadas de ejercicios de Lectures on the Curry Howard Isomorphism
-- Originalmente son para deducción natural de intuicionista.

-- Dado un id de predicado devuelve un predicado de aridad 0,
-- i.e una variable proposicional
propVar :: PredId -> Form 
propVar pid = FPred pid []

predVar :: PredId -> VarId -> Form
predVar p v = FPred p [TVar v]

-- A -> A
f1 :: Form
f1 = FImp (propVar "A") (propVar "A")

p1 :: Proof
p1 = PImpI "hA" (PAx "hA")

-- A -> (B -> A)
f2 :: Form
f2 = FImp
    (propVar "A")
    (FImp
        (propVar "B")
        (propVar "A")
    )

p2 :: Proof
p2 = PImpI "hA" (PImpI "hB" (PAx "hA"))

-- A -> (B -> B)
f3 :: Form
f3 = FImp
    (propVar "A")
    (FImp
        (propVar "B")
        (propVar "B")
    )

p3 :: Proof
p3 = PImpI "x" (PImpI "x" (PAx "x"))

-- (A -> (B -> C)) -> [(A -> B) -> (A -> C)]
f4 :: Form
f4 = FImp
        (FImp
            (propVar "A")
            (FImp (propVar "B") (propVar "C"))
        )
        (FImp
            (FImp (propVar "A") (propVar "B"))
            (FImp (propVar "A") (propVar "C"))
        )


p4 :: Proof
p4 = PImpI "h A -> (B -> C)" (
        PImpI "h A -> B" (
            PImpI "h A" (
                -- B -> C
                PImpE
                    (propVar "B")
                    -- Dem B -> C por A -> (B -> C)
                    (PImpE
                        (propVar "A")
                        (PAx "h A -> (B -> C)")
                        (PAx "h A")
                    )
                    -- Dem B por A -> B
                    (PImpE
                        (propVar "A")
                        (PAx "h A -> B")
                        (PAx "h A")
                    )
            )
        )
    )

-- Errores en ambos juicios
p4Err1 :: Proof
p4Err1 = PImpI "h A -> (B -> C)" (
        PImpI "h A -> B" (
            PImpI "h A" (
                -- B -> C
                PImpE
                    (propVar "B")
                    -- Dem B -> C errónea, no hay hyp
                    (PAx "h B -> C")
                    -- Dem B por A -> B
                    (PImpE
                        (propVar "A")
                        (PAx "h A -> B")
                        (PAx "h A")
                    )
            )
        )
    )

p4Err2 :: Proof
p4Err2 = PImpI "h A -> (B -> C)" (
        PImpI "h A -> B" (
            PImpI "h A" (
                -- B -> C
                PImpE
                    (propVar "B")
                    -- Dem B -> C por A -> (B -> C)
                    (PImpE
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
p5 = PImpI "h False" (
        PFalseE (PAx "h False")
    )

-- p -> ~~p
f6 :: Form
f6 = FImp (propVar "P") (FNot $ FNot $ propVar "P")

p6 :: Proof
p6 = PImpI "h P" (
        PNotI "h ~P" (
            PNotE
                (propVar "P")
                (PAx "h ~P")
                (PAx "h P")
        )
    )

-- ~~~p -> ~p
f7 :: Form
f7 = FImp (FNot $ FNot $ FNot $ propVar "P") (FNot $ propVar "P")

p7 :: Proof
p7 = PImpI "h ~~~P" (
        PNotI "h P" (
            PNotE
                (FNot $ FNot $ propVar "P")
                -- ~~~P
                (PAx "h ~~~P")
                -- ~~P
                (PNotI "h ~P" (
                    PNotE
                        (propVar "P")
                        (PAx "h ~P")
                        (PAx "h P")
                ))
        )
    )

-- modus tollens
-- (ej7 curry howard) (A -> B) -> (~B -> ~A)
f8 :: Form
f8 = FImp (FImp (propVar "A")
                (propVar "B"))
          (FImp (FNot $ propVar "B")
                (FNot $ propVar "A"))

p8 :: Proof
p8 = PImpI "h A -> B" (
        PImpI "h ~B" (
            PNotI "h A" (
                PNotE
                    (propVar "B")
                    (PAx "h ~B")
                    -- dem B con A -> B
                    (PImpE
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
p9 = PImpI "h ~~A" (
        -- Uso LEM de A v ~A
        POrE
            (propVar "A") (FNot $ propVar "A")
            PLEM
            -- Dem de A asumiendo A
            "h A" (PAx "h A")
            -- Dem de A asumiendo ~ A
            "h ~A" (
                -- ~A y ~~A generan una contradicción
                PFalseE (
                    PNotE
                        (FNot $ propVar "A") -- Uso ~~A
                        -- Dem de ~~A
                        (PAx "h ~~A")
                        -- Dem de ~A
                        (PAx "h ~A")
                )
            )
    )

-- Dada una fórmula A da su doble negación
dneg :: Form -> Form
dneg f = FNot $ FNot f

-- Dada una fórmula A da una demostración de ~~A -> A
doubleNegElim :: Form -> Proof
doubleNegElim formA =
    PImpI "h ~~{A}" (
        -- Uso LEM de A v ~A
        POrE
            formA (FNot formA)
            PLEM
            -- Dem de A asumiendo A
            "h {A}" (PAx "h {A}")
            -- Dem de A asumiendo ~ A
            "h ~{A}" (
                -- ~A y ~~A generan una contradicción
                PFalseE (
                    PNotE
                        (FNot formA) -- Uso ~~A
                        -- Dem de ~~A
                        (PAx "h ~~{A}")
                        -- Dem de ~A
                        (PAx "h ~{A}")
                )
            )
        )


-- doubleNegElim de A se puede usar para demostrar A por contradicción, asumiendo ~A
{-
    PImpE
        dneg A
        dnegEim A
        -- Proof de ~~A
        PNotI "h ~A"
            -- Proof de bottom asumiendo ~A, es decir que asumir que no vale A
            -- lleva a una contradicción.
-}


-- De morgan

-- (ej9 CurryHoward) (~A v ~B) -> ~(A ^ B)

f10 :: Form
f10 = FImp
        (FOr (FNot $ propVar "A") (FNot $ propVar "B"))
        (FNot $ FAnd (propVar "A") (propVar "B"))

p10 :: Proof
p10 = PImpI "h ~A v ~B" (
        PNotI "h A ^ B" (
            -- Para demostrar ~(A^B), asumimos que no vale y dem false
            -- Para demostrar false, por casos en h ~A v ~B. En cualquiera
            -- llegamos a una contradicción con h A ^ B
            POrE
                (FNot $ propVar "A") (FNot $ propVar "B") (PAx "h ~A v ~B")
                "h ~A" (
                    PNotE
                        (propVar "A") -- uso ~A
                        (PAx "h ~A")
                        (PAndE1 (propVar "B") (PAx "h A ^ B"))
                )
                "h ~B" (
                    PNotE
                        (propVar "B") -- uso ~B
                        (PAx "h ~B")
                        (PAndE2 (propVar "A") (PAx "h A ^ B"))
                )

        )
    )

-- ej 11 CurryHoward, curryficación
-- ((A ^ B) -> C) <-> (A -> (B -> C))

f11 :: Form
f11 = FAnd
        (FImp
            (FImp (FAnd (propVar "A") (propVar "B")) (propVar "C"))
            (FImp (propVar "A") (FImp (propVar "B") (propVar "C"))))
         (FImp
            (FImp (propVar "A") (FImp (propVar "B") (propVar "C")))
            (FImp (FAnd (propVar "A") (propVar "B")) (propVar "C")))

p11 :: Proof
p11 = PAndI
        -- ((A ^ B) -> C) -> (A -> (B -> C))
        (PImpI "h (A ^ B) -> C)" (
            PImpI "h A" (
                PImpI "h B" (
                    PImpE
                        (FAnd (propVar "A") (propVar "B"))
                        (PAx "h (A ^ B) -> C)")
                        (PAndI (PAx "h A") (PAx "h B"))
                ) 
            )
        ))

        -- (A -> (B -> C)) -> ((A ^ B) -> C)
        (PImpI "h (A -> (B -> C))" (
            PImpI "h A ^ B" (
                -- Implico C a partir de B -> C que viene de A -> (B -> C)
                PImpE
                    (propVar "B")
                    -- Interesante que la dem de B -> C no es PAx
                    (PImpE
                        (propVar "A") -- A -> (B -> C)
                        (PAx "h (A -> (B -> C))")
                        (PAndE1 (propVar "B") (PAx "h A ^ B")))
                    (PAndE2 (propVar "A") (PAx "h A ^ B"))
            )
        ))

-- ej 13 CurryHoward
-- ~~(A v ~A)
f12 :: Form
f12 = FNot $ FNot $ FOr (propVar "A") (FNot $ propVar "A")

p12LEM :: Proof
p12LEM = PNotI "h ~(A v ~A)" (
        PNotE
            (FOr (propVar "A") (FNot $ propVar "A"))
            -- ~(A v ~A)
            (PAx "h ~(A v ~A)")
            -- A v ~A
            -- medio trucho
            PLEM
    )

p12 :: Proof 
p12 = PNotI "h ~(A v ~A)" (
        PNotE
            (FOr (propVar "A") (FNot $ propVar "A"))
            -- ~(A v ~A)
            (PAx "h ~(A v ~A)")
            -- A v ~A
            (POrI2 (PNotI "h A" (
                PNotE
                    (FOr (propVar "A") (FNot $ propVar "A"))
                    (PAx "h ~(A v ~A)")
                    (POrI1 (PAx "h A"))
            )))
    )

-- alguna usando true
-- (A ^ true) <-> A
f13 :: Form
f13 = FAnd
        (FImp
            (FAnd (propVar "A") FTrue)
            (propVar "A"))
        (FImp
            (propVar "A")
            (FAnd (propVar "A") FTrue))

p13 :: Proof
p13 = PAndI
        -- A ^ true -> A
        (PImpI "h A ^ true" (
            PAndE1 FTrue (PAx "h A ^ true")
        ))
        -- A -> A ^ true
        (PImpI "h A" (
            PAndI
                (PAx "h A")
                PTrueI
        ))

-- (A v true) <-> true
f14 :: Form
f14 = FAnd
        (FImp
            (FOr (propVar "A") FTrue)
            FTrue)
        (FImp
            FTrue
            (FOr (propVar "A") FTrue))

p14 :: Proof
p14 = PAndI
        -- A v true -> true
        (PImpI "h A v true" (
            POrE
                (propVar "A") FTrue
                (PAx "h A v true")
                "h A" PTrueI
                "h true" PTrueI
        ))
        -- true -> A v true
        (PImpI "h true" (
            POrI2 $ PAx "h true"
        ))

-- vuelta (solo LK)
-- ~(A ^ B) -> (~A v ~B)
f15 :: Form
f15 = FImp
        (FNot $ FAnd (propVar "A") (propVar "B"))
        (FOr (FNot $ propVar "A") (FNot $ propVar "B"))

-- Estrategia: usar eliminación de la doble negación, y después se puede hacer
-- una dem intuicionista
-- Tengo una dem 100% clásica en el cuaderno pero es muy larga
p15 :: Proof
p15 = PImpI "h ~(A ^ B)" (
        -- Uso eliminación de doble negación
        -- ~~(~A v ~B) -> ~A v ~B
        PImpE
            (dneg $ FOr (FNot fA) (FNot fB))
            (doubleNegElim $ FOr (FNot fA) (FNot fB))
            -- Dem de ~~(~A v ~B)
            (PNotI "h ~(~A v ~B)" (
                -- Acá es una dem por el absurdo, asumimos ~(~A v ~B) que es
                -- la negación que lo que queremos probar, y llegamos a bottom.
                PNotE
                    (FOr (FNot fA) (FNot fB))
                    (PAx "h ~(~A v ~B)")
                    (POrI1 (
                        PNotI "h A" (
                            PNotE
                                (FOr (FNot fA) (FNot fB))
                                (PAx "h ~(~A v ~B)")
                                (POrI2 (
                                    PNotI "h B" (
                                        PNotE
                                            (FAnd fA fB)
                                            (PAx "h ~(A ^ B)")
                                            (PAndI
                                                (PAx "h A")
                                                (PAx "h B"))
                                    )
                                ))
                        )
                    ))
            ))
    )
    where fA = propVar "A"
          fB = propVar "B"

-- ~(A v B) <-> ~A ^ ~B

-- ~(A v B) -> ~A ^ ~B
f16 :: Form
f16 = FImp (FNot $ FOr fA fB )
           (FAnd (FNot fA) (FNot fB))
    where fA = propVar "A"
          fB = propVar "B"

p16 :: Proof
p16 = PImpI "h ~(A v B)" (
        PAndI
            -- Dem ~A
            (PNotI "h A" (
                PNotE
                    (FOr fA fB)
                    (PAx "h ~(A v B)")
                    (POrI1 (PAx "h A"))
            ))
            -- Dem ~B
            (PNotI "h B" (
                PNotE
                    (FOr fA fB)
                    (PAx "h ~(A v B)")
                    (POrI2 (PAx "h B"))
            ))
    )
    where fA = propVar "A"
          fB = propVar "B"


-- ~A ^ ~B -> ~(A v B)
f17 :: Form
f17 = FImp (FAnd (FNot fA) (FNot fB))
           (FNot $ FOr fA fB )
    where fA = propVar "A"
          fB = propVar "B"

p17 :: Proof
p17 = PImpI "h ~A ^ ~B" (
        PNotI "h A v B" (
            -- dem de bottom (contradicción)
            -- idea: asumo A, por ~A contradicción. Análogo para B y ~B
            POrE
                fA fB (PAx "h A v B")
                "h A"
                -- proof de bottom asumiendo A
                (PNotE
                    fA
                    (PAndE1 (FNot fB) (PAx "h ~A ^ ~B"))
                    (PAx "h A"))
                -- proof de bot asumiendo B
                "h B"
                (PNotE
                    fB
                    (PAndE2 (FNot fA) (PAx "h ~A ^ ~B"))
                    (PAx "h B"))
        )
    )
    where fA = propVar "A"
          fB = propVar "B"


-- Dems bobas con exists y forall

-- Good(y) -> Exists x. Good(x)
f18 :: Form
f18 = FImp (predVar "Good" "y")
           (FExists "x" (predVar "Good" "x"))

p18 :: Proof
p18 = PImpI "h Good(y)" (
        PExistsI
            (TVar "y")
            (PAx "h Good(y)")
    )

-- Exists x. A(x) ^ B(x) -> Exists y. A(y)
f19 :: Form
f19 = FImp
        (FExists "x" (FAnd 
            (predVar "A" "x")
            (predVar "B" "x")))
        (FExists "y" (predVar "A" "y"))

p19 :: Proof
p19 = PImpI "h Exists x. A(x) ^ B(x)" (
        PExistsE
            "x"
            (FAnd 
                (predVar "A" "x")
                (predVar "B" "x"))
            (PAx "h Exists x. A(x) ^ B(x)")
            "h A(x) ^ B(x)"
            -- Dem Exists y. A(y)
            (PExistsI
                (TVar "x")
                -- Dem A(x)
                (PAndE1
                    (predVar "B" "x")
                    (PAx "h A(x) ^ B(x)")))
    )

-- Es necesario primero hacer eliminación del exists y después PExistsI, PAndE1, porque sino al revés
-- se querría probar A(x) ^ B(x) con ExistsE pero no se puede porque tiene x libre.
-- Análogamente, para probar lo mismo pero del consecuente Exists x. A(x), no hay problema porque justamente x no está libre, sino que está ligado por el existencial.

-- Forall x. Good(x) -> Good(y)

-- Misma x, tiene que funcionar porque no está libre
-- Forall x. A(x) ^ B(x) => Forall x. A(x)
f20 :: Form
f20 = FImp
        (FForall "x" (FAnd ax bx))
        (FForall "x" ax)
    where ax = FPred "A" [TVar "x"]
          bx = FPred "B" [TVar "x"]

p20 :: Proof
p20 = PImpI "h Forall x. A(x) ^ B(x)" (
        PForallI (
            -- Proof A(x)
            PAndE1
                bx
                (PForallE
                    "x" (FAnd ax bx)
                    (PAx "h Forall x. A(x) ^ B(x)")
                    (TVar "x"))
        )
    )
    where ax = FPred "A" [TVar "x"]
          bx = FPred "B" [TVar "x"]

-- Var diferente, debería ser lo mismo
-- Forall x. A(x) ^ B(x) => Forall y. A(y)
f20' :: Form
f20' = FImp
        (FForall "x" (FAnd ax bx))
        (FForall "y" ay)
    where ax = FPred "A" [TVar "x"]
          ay = FPred "A" [TVar "y"]
          bx = FPred "B" [TVar "x"]

p20' :: Proof
p20' = PImpI "h Forall x. A(x) ^ B(x)" (
        PForallI (
            -- Proof A(y)
            PAndE1
                by -- tengo que cambiar en ambos
                (PForallE
                    "x" (FAnd ax bx)
                    (PAx "h Forall x. A(x) ^ B(x)")
                    (TVar "y"))
        )
    )
    where ax = FPred "A" [TVar "x"]
          bx = FPred "B" [TVar "x"]
          by = FPred "B" [TVar "y"]


-- Dem inválida de introducción forall, en la que no está libre x en el contexto
-- A(x) => Forall x. A(x)
-- no está bien, y se podría demostrar con PAx
f21 :: Form
f21 = FImp (FPred "A" [ TVar "x" ])
           (FForall "x" (FPred "A" [ TVar "x" ]))

p21 :: Proof
p21 = PImpI "h A(x)" (PForallI (PAx "h A(x)"))

-- Dem inválida de eliminación de forall, en donde el término a demostrar no
-- A{x := t} sino que otra cosa
-- Forall x. A(x) => Exists x. B(x)
f22 :: Form
f22 = FImp
        (FForall "x" $ FPred "A" [TVar "x"])
        (FExists "x" $ FPred "B" [TVar "x"])

p22 :: Proof
p22 = PImpI "h Forall x. A(x)" (
        PExistsI (TVar "x") (
            PForallE
                "x" (FPred "A" [TVar "x"])
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
f23Ida = FImp
            (FForall "x" $ predVar "A" "x")
            (FNot $ FExists "x" $ FNot $ predVar "A" "x")

-- No se puede invertir el orden de NotE y ExistsE porque si hiciera NotE
-- primero, tendría que demostrar A(x) con ExistsE y no cumpliría con la
-- restricción de las FV

p23Ida :: Proof
p23Ida = PImpI "h V x. A(x)" (
            PNotI "h E x. ~A(x)" (
                -- Dem bot
                PExistsE
                    "x" (FNot $ predVar "A" "x")
                    (PAx "h E x. ~A(x)")
                    "h ~A(x)"
                    -- Contradicción de ~A(x) y A(x)
                    (PNotE
                        (predVar "A" "x")
                        (PAx "h ~A(x)")
                        -- Instancio V x. A(x) en x para llegar al abs
                        (PForallE
                            "x" (predVar "A" "x")
                            (PAx "h V x. A(x)")
                            (TVar "x") -- no cambia x
                            ))
            )
        )

-- Esta pinta que va a ser la difícil
-- ~E x. ~A(x) => V x. A(x)
f23Vuelta :: Form
f23Vuelta = FImp
                (FNot $ FExists "x" $ FNot $ predVar "A" "x")
                (FForall "x" $ predVar "A" "x")

p23Vuelta :: Proof
p23Vuelta = PImpI "h ~E x. ~A(x)" (
                PForallI (
                    -- Dem de A(x), por absurdo, asumo ~A(x) mediante dnegelim
                    PImpE
                        (dneg $ predVar "A" "x")
                        (doubleNegElim $ predVar "A" "x")
                        -- Dem ~~A(x)
                        (PNotI "h ~A(x)" (
                            PNotE
                                (FExists "x" $ FNot $ predVar "A" "x")
                                (PAx "h ~E x. ~A(x)")
                                -- Dem E x. ~A(x)
                                (PExistsI
                                    (TVar "x")
                                    (PAx "h ~A(x)"))
                        ))
                )
            )

-- E x. A(x) => ~ V x. ~A(x)
f24Ida :: Form
f24Ida = FImp
            (FExists "x" $ predVar "A" "x")
            (FNot $ FForall "x" $ FNot $ predVar "A" "x")

p24Ida :: Proof
p24Ida = PImpI "h E x. A(x)" (
            PNotI "h V x. ~A(x)" (
                PExistsE
                    "x" (predVar "A" "x")
                    (PAx "h E x. A(x)")
                    "h A(x)"
                    (PNotE
                        (predVar "A" "x")
                        (PForallE
                            "x" (FNot $ predVar "A" "x")
                            (PAx "h V x. ~A(x)")
                            (TVar "x") -- queda igual
                        )
                        (PAx "h A(x)")
                    )
            )
        )

-- ~ V x. ~A(x) => E x. A(x)
f24Vuelta :: Form
f24Vuelta = FImp
                (FNot $ FForall "x" $ FNot $ predVar "A" "x")
                (FExists "x" $ predVar "A" "x")

-- Acá no funciona primero hacer ExistsI y después dneg sobre A(x) porque el
-- absurdo es sobre la no existencia
p24Vuelta :: Proof
p24Vuelta =
    PImpI "h ~V x. ~A(x)" (
        -- Dem E x. A(x) por absurdo mediante dneg elim, asumo ~E x. A(x)
        PImpE
            (dneg $ FExists "x" $ predVar "A" "x")
            (doubleNegElim $ FExists "x" $ predVar "A" "x")
            (PNotI "h ~E x. A(x)" (
                PNotE
                    (FForall "x" (FNot $ predVar "A" "x"))
                    (PAx "h ~V x. ~A(x)")
                    -- Dem de V x. ~A(x) (usando que no existe x. A(x))
                    (PForallI (
                        PNotI "h A(x)" (
                            PNotE
                                (FExists "x" $ predVar "A" "x")
                                (PAx "h ~E x. A(x)")
                                (PExistsI (TVar "x") (PAx "h A(x)"))
                        )
                    ))
            ))
    )

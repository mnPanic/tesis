module Proofs where

import Prover
    ( Proof(PAx, PImpE, PLEM, PFalseE, PImpI, PNotI, POrE, PAndE1,
            PNotE, PAndE2, PAndI, POrI2, POrI1, PTrueI, PExistsI),
      Form(FNot, FPred, FFalse, FImp, FOr, FAnd, FTrue, FForall, FExists),
      PredId, Term (TVar) )

-- Dems sacadas de ejercicios de Lectures on the Curry Howard Isomorphism
-- Originalmente son para deducción natural de intuicionista.

-- Dado un id de predicado devuelve un predicado de aridad 0,
-- i.e una variable proposicional
propVar :: PredId -> Form 
propVar pid = FPred pid []

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
            (FNot $ FNot $ FOr (FNot fA) (FNot fB))
            (doubleNegElim $ FOr (FNot fA) (FNot fB))
            -- Dem de ~~(~A v ~B)
            (PNotI "h (~A v ~B)" (
                PNotE
                    (FOr (FNot fA) (FNot fB))
                    (PAx "h (~A v ~B)")
                    (POrI1 (
                        PNotI "h A" (
                            PNotE
                                (FOr (FNot fA) (FNot fB))
                                (PAx "h (~A v ~B)")
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
-- TODO

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
f18 = FImp (FPred "Good" [ TVar "y" ])
           (FExists "x" (FPred "Good" [ TVar "x" ]))

p18 :: Proof
p18 = PImpI "h Good(y)" (
        PExistsI
            (TVar "y")
            (PAx "h Good(y)")
    )

-- Forall x. Good(x) -> Good(y)

-- TODO leyes de demorgan (son dificiles - pablo)
-- ~forall <=> exists~
-- ~exists <=> forall~
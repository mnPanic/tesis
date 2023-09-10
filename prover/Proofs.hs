module Proofs where

import Prover
    ( Proof(PAx, PImpE, PLEM, PFalseE, PImpI, PNotI, POrE, PAndE1,
            PNotE, PAndE2, PAndI, POrI2, POrI1, PTrueI),
      Form(FNot, FPred, FFalse, FImp, FOr, FAnd, FTrue),
      PredId )

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

p15 :: Proof
p15 = PImpI "h ~(A ^ B)" (
        
    )
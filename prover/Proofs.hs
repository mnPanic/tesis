module Proofs where

import Prover

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

p2 = PImpI "hA" (PImpI "hB" (PAx "hA"))

-- A -> (B -> B)
f3 = FImp
    (propVar "A")
    (FImp
        (propVar "B")
        (propVar "B")
    )
p3 = PImpI "x" (PImpI "x" (PAx "x"))

-- (A -> (B -> C)) -> [(A -> B) -> (A -> C)]
f4 = FImp
        (FImp
            (propVar "A")
            (FImp (propVar "B") (propVar "C"))
        )
        (FImp
            (FImp (propVar "A") (propVar "B"))
            (FImp (propVar "A") (propVar "C"))
        )


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
f5 = FImp FFalse $ propVar "P"
p5 = PImpI "h False" (
        PFalseE (PAx "h False")
    )

-- p -> ~~p
f6 = FImp (propVar "P") (FNot $ FNot $ propVar "P")
p6 = PImpI "h P" (
        PNotI "h ~P" (
            PNotE
                (propVar "P")
                (PAx "h ~P")
                (PAx "h P")
        )
    )

-- ~~~p -> ~p
f7 = FImp (FNot $ FNot $ FNot $ propVar "P") (FNot $ propVar "P")
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
f8 = FImp (FImp (propVar "A")
                (propVar "B"))
          (FImp (FNot $ propVar "B")
                (FNot $ propVar "A"))
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

-- ~~p -> p, si vale para LK
f9 = FImp (FNot $ FNot $ propVar "A") (propVar "A")

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

f10 = FImp
        (FOr (FNot $ propVar "A") (FNot $ propVar "B"))
        (FNot $ FAnd (propVar "A") (propVar "B"))

p10 = PImpI ("h ~A v ~B") (
        PNotI ("h A ^ B") (
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

-- vuelta (solo LK)
-- ~(A ^ B) -> (~A v ~B)
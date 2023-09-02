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
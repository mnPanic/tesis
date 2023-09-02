module Proofs where

import Prover
-- A -> A
f1 :: Form
f1 = FImp (FPred "A" []) (FPred "A" [])

p1 :: Proof
p1 = PImpI "hA" (PAx "hA")

-- A -> (B -> A)
f2 :: Form
f2 = FImp
    (FPred "A" [])
    (FImp
        (FPred "B" [])
        (FPred "A" [])
    )

p2 = PImpI "hA" (PImpI "hB" (PAx "hA"))

-- A -> (B -> B)
f3 = FImp
    (FPred "A" [])
    (FImp
        (FPred "B" [])
        (FPred "B" [])
    )
p3 = PImpI "x" (PImpI "x" (PAx "x"))
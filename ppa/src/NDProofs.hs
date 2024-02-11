-- Contains proof macros / generators
module NDProofs (proofAndEProjection) where

import ND (
    Form (..),
    HypId,
    Proof (..),
 )

import Text.Printf (printf)

type Result a = Either String a

-- proofAndEProjection dada una cláusula (a_1 ^ ... ^ a_n), que puede estar
-- con asociada de cualquier manera, devuelve una demostración de
--  (a_1 ^ ... ^ a_n) |- a_i
-- con i de 1 a n
proofAndEProjection :: Form -> HypId -> Form -> Result Proof
proofAndEProjection fAnd hAnd f =
    case proj' fAnd f of
        Right p -> Right $ p (PAx hAnd)
        Left e -> Left e

proj' :: Form -> Form -> Result (Proof -> Proof)
proj' fAnd@(FAnd l r) f = case proj' l f of
    Right p -> Right $ \next -> p (PAndE1 r next)
    Left el -> case proj' r f of
        Left er ->
            Left
                $ printf
                    "%s |- %s not possible by left (%s) or right (%s)"
                    (show fAnd)
                    (show f)
                    el
                    er
        Right p -> Right $ \next -> p (PAndE2 l next)
proj' f1 f2
    | f1 == f2 = Right id
    | otherwise = Left $ printf "%s /= %s" (show f1) (show f2)
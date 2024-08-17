{- TODOs
- Validar que R no tenga FVs (porque sino hay que hacer libre de captura, y no
  debería ser necesario)

-}
module NDExtractor (translateF, translateP) where

import ND (Form (..), Proof (..), Term)
import NDProofs (Result)
import NDReducer (reduce)
import Text.Printf (printf)

{- Dada una demostración de exists X . p(X) devuelve t tal que p(t)
Para ello,
- (wip) traduce la demostración de clásica a intuicionista usando la traducción de Friedman
- reduce la demostración
- si el resultado es una
La fórmula debe ser de la clase Sigma^0_1, es decir N existenciales seguidos de una fórmula sin cuantificadores.
-}
extractWitness :: Proof -> Form -> Result Term
extractWitness proof (FExists x f) = do
    -- TODO: Friedman translation
    let reducedProof = reduce proof
    case reducedProof of
        (PExistsI t _) -> Right t
        proof' -> Left "proof not exists introduction, can't extract witness"
extractWitness _ f = Left $ printf "form %s must be exists" (show f)

-- Convierte una demostración clásica en una intuicionista usando la traducción de friedman.
translateP :: Proof -> Proof
translateP = undefined

-- translateP p = case p of
--     PAx h -> PAx h

-- Traduce f via doble negación relativizada, parametrizada por una fórmula
-- arbitraria R.
-- FNot a ~~> FImp a r (FNot_R)
translateF :: Form -> Form -> Form
translateF f r = case f of
    FAnd l r -> FAnd (rec l) (rec r)
    FOr l r -> fNotR (FAnd (fNotR (rec l)) (fNotR (rec r)))
    FImp l r -> FImp (rec l) (rec r)
    FNot g -> fNotR (rec g)
    FForall x g -> FForall x (rec g)
    FExists x g -> fNotR (FForall x (fNotR (rec g)))
    FFalse -> r
    FTrue -> FTrue
    f@(FPred{}) -> fNotR (fNotR f)
  where
    rec g = translateF g r
    fNotR f = FImp f r
{- TODOs
- Validar que R no tenga FVs (porque sino hay que hacer libre de captura, y no
  debería ser necesario)

-}
module NDExtractor (translateF, translateP) where

import Debug.Trace (trace)
import ND (Form (..), Proof (..), Term, proofName)
import NDProofs (Result, hypForm)
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
translateP :: Proof -> Form -> Form -> (Proof, Form)
translateP _ _ _ | trace "translateP" False = undefined
translateP proof form r = case (form, proof) of
  (f, PAx h) -> (PAx h, translateF f r)
  (f, PNamed n p) ->
    let
      (p', f') = rec p f
     in
      (PNamed n p', f')
  {- Imp -}
  ( cons
    , PImpE
        { antecedent = ant
        , proofImp = proofImp
        , proofAntecedent = proofAnt
        }
    ) ->
      let imp = FImp ant cons
          (proofImp', _) = rec proofImp imp
          (proofAnt', ant') = rec proofAnt ant
          cons' = translateF cons r
          proofCons' =
            PImpE
              { antecedent = ant'
              , proofImp = proofImp'
              , proofAntecedent = proofAnt'
              }
       in (proofCons', cons')
  ( imp@(FImp ant cons)
    , PImpI
        { hypAntecedent = h
        , proofConsequent = proofCons
        }
    ) ->
      let
        (proofCons', _) = rec proofCons cons
        imp' = translateF imp r
        proofImp' =
          PImpI
            { hypAntecedent = h
            , proofConsequent = proofCons'
            }
       in
        (proofImp', imp')
  {- And -}
  ( and@(FAnd left right)
    , PAndI
        { proofLeft = proofL
        , proofRight = proofR
        }
    ) ->
      let
        (proofLeft', _) = rec proofL left
        (proofRight', _) = rec proofR right
        and' = translateF and right
        proofAnd' =
          PAndI
            { proofLeft = proofLeft'
            , proofRight = proofRight'
            }
       in
        (proofAnd', and')
  ( left
    , PAndE1
        { right = right
        , proofAnd = proofAnd
        }
    ) ->
      let
        and = FAnd left right
        (proofAnd', _) = rec proofAnd and
        left' = translateF left r
        right' = translateF right r
        proofL' =
          PAndE1
            { right = right'
            , proofAnd = proofAnd'
            }
       in
        (proofL', left')
  ( right
    , PAndE2
        { left = left
        , proofAnd = proofAnd
        }
    ) ->
      let
        and = FAnd left right
        (proofAnd', _) = rec proofAnd and
        left' = translateF left r
        right' = translateF right r
        proofR' =
          PAndE2
            { left = left'
            , proofAnd = proofAnd'
            }
       in
        (proofR', right')
  {- False -}
  {- True -}
  {- LEM -}
  (or@(FOr f (FNot g)), PLEM) ->
    -- ~R (~R f~~ & ~R~R f~~)
    case translateF or r of
      or'@(FImp and@(FAnd left right) r) ->
        let
          proofOr' =
            PImpI
              { -- ~R f~~ & ~R~R f~~
                hypAntecedent = hypForm and
              , -- R
                proofConsequent =
                  PImpE
                    { antecedent = right
                    , proofImp =
                        PAndE2
                          { left = left
                          , proofAnd = PAx $ hypForm and
                          }
                    , -- ~R f~~
                      proofAntecedent =
                        PAndE1
                          { right = right
                          , proofAnd = PAx $ hypForm and
                          }
                    }
              }
         in
          (proofOr', or')
      or' -> error ("unexpected format " ++ show or')
  {- Or -}
  {- Forall -}
  {- Exists -}
  {- Not -}
  (f, p) -> error $ printf "translateP: unexpected proof %s for form %s" (proofName p) (show f)
 where
  rec p f = translateP p f r

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
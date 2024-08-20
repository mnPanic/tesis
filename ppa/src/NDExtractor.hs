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
-- translateP _ _ _ | trace "translateP" False = undefined
translateP proof form r = case proof of
  PAx h -> (PAx h, translateF form r)
  PNamed n p ->
    let (p', f') = translateP p form r
     in (PNamed n p', f')
  {- Imp -}
  PImpE{} -> translateImpE form proof r
  PImpI{} -> translateImpI form proof r
  {- And -}
  PAndI{} -> translateAndI form proof r
  PAndE1{} -> translateAndE1 form proof r
  PAndE2{} -> translateAndE2 form proof r
  {- False -}
  {- True -}
  {- LEM -}
  PLEM -> translateLEM form proof r
  {- Or -}
  POrI1{} -> translateOrI1 form proof r
  POrI2{} -> translateOrI2 form proof r
  {- Forall -}
  PForallI{} -> translateForallI form proof r
  PForallE{} -> translateForallE form proof r
  {- Exists -}
  {- Not -}
  p -> error $ printf "translateP: unexpected proof %s for form %s" (proofName p) (show form)

translateImpI :: Form -> Proof -> Form -> (Proof, Form)
translateImpI
  imp@(FImp ant cons)
  PImpI
    { hypAntecedent = h
    , proofConsequent = proofCons
    }
  r =
    let
      (proofCons', _) = translateP proofCons cons r
      imp' = translateF imp r
      proofImp' =
        PImpI
          { hypAntecedent = h
          , proofConsequent = proofCons'
          }
     in
      (proofImp', imp')

translateImpE :: Form -> Proof -> Form -> (Proof, Form)
translateImpE
  cons
  PImpE
    { antecedent = ant
    , proofImp = proofImp
    , proofAntecedent = proofAnt
    }
  r =
    let
      imp = FImp ant cons
      (proofImp', _) = translateP proofImp imp r
      (proofAnt', ant') = translateP proofAnt ant r
      cons' = translateF cons r
      proofCons' =
        PImpE
          { antecedent = ant'
          , proofImp = proofImp'
          , proofAntecedent = proofAnt'
          }
     in
      (proofCons', cons')

translateAndI :: Form -> Proof -> Form -> (Proof, Form)
translateAndI
  and@(FAnd left right)
  PAndI
    { proofLeft = proofL
    , proofRight = proofR
    }
  r =
    let
      (proofLeft', _) = translateP proofL left r
      (proofRight', _) = translateP proofR right r
      and' = translateF and right
      proofAnd' =
        PAndI
          { proofLeft = proofLeft'
          , proofRight = proofRight'
          }
     in
      (proofAnd', and')

translateAndE1 :: Form -> Proof -> Form -> (Proof, Form)
translateAndE1
  left
  PAndE1
    { right = right
    , proofAnd = proofAnd
    }
  r =
    let
      and = FAnd left right
      (proofAnd', _) = translateP proofAnd and r
      left' = translateF left r
      right' = translateF right r
      proofL' =
        PAndE1
          { right = right'
          , proofAnd = proofAnd'
          }
     in
      (proofL', left')

translateAndE2 :: Form -> Proof -> Form -> (Proof, Form)
translateAndE2
  right
  PAndE2
    { left = left
    , proofAnd = proofAnd
    }
  r =
    let
      and = FAnd left right
      (proofAnd', _) = translateP proofAnd and r
      left' = translateF left r
      right' = translateF right r
      proofR' =
        PAndE2
          { left = left'
          , proofAnd = proofAnd'
          }
     in
      (proofR', right')

translateLEM :: Form -> Proof -> Form -> (Proof, Form)
translateLEM or@(FOr f (FNot g)) PLEM r =
  -- ~R (~R f~~ & ~R~R f~~)
  case translateF or r of
    or'@(FImp and@(FAnd left right) r) ->
      let
        hAnd = hypForm and
        proofOr' =
          PImpI
            { -- ~R f~~ & ~R~R f~~
              hypAntecedent = hAnd
            , -- R
              proofConsequent =
                -- Elimino ~R~R f~~, porque sabemos que vale ~R f~~
                PImpE
                  { antecedent = left
                  , proofImp =
                      PAndE2
                        { left = left
                        , proofAnd = PAx hAnd
                        }
                  , -- ~R f~~
                    proofAntecedent =
                      PAndE1
                        { right = right
                        , proofAnd = PAx hAnd
                        }
                  }
            }
       in
        (proofOr', or')
    or' -> error ("unexpected format " ++ show or')

translateOrI1 :: Form -> Proof -> Form -> (Proof, Form)
translateOrI1
  or@(FOr left right)
  POrI1
    { proofLeft = proofLeft
    }
  r =
    -- ~r (~r left~~ & ~r right~~)
    case translateF or r of
      or'@(FImp and@(FAnd (FImp left' _) notR_right') r) ->
        let
          (proofLeft', _) = translateP proofLeft left r
          hAnd = hypForm and
          proofOr' =
            PImpI
              { hypAntecedent = hAnd
              , proofConsequent =
                  PImpE
                    { antecedent = left'
                    , proofImp =
                        PAndE1
                          { right = notR_right'
                          , proofAnd = PAx hAnd
                          }
                    , proofAntecedent = proofLeft'
                    }
              }
         in
          (proofOr', or')
      f' -> error ("unexpected format " ++ show f')

translateOrI2 :: Form -> Proof -> Form -> (Proof, Form)
translateOrI2
  or@(FOr left right)
  POrI2
    { proofRight = proofRight
    }
  r =
    -- ~r (~r left~~ & ~r right~~)
    case translateF or r of
      or'@(FImp and@(FAnd notR_left' (FImp right' _)) r) ->
        let
          (proofRight', _) = translateP proofRight right r
          hAnd = hypForm and
          proofOr' =
            PImpI
              { hypAntecedent = hAnd
              , proofConsequent =
                  PImpE
                    { antecedent = right'
                    , proofImp =
                        PAndE2
                          { left = notR_left'
                          , proofAnd = PAx hAnd
                          }
                    , proofAntecedent = proofRight'
                    }
              }
         in
          (proofOr', or')
      f' -> error ("unexpected format " ++ show f')

translateForallI :: Form -> Proof -> Form -> (Proof, Form)
translateForallI
  _forall@(FForall x f)
  PForallI
    { newVar = x'
    , proofForm = proofForm
    }
  r =
    let
      forall' = translateF _forall r
      (proofForm', _) = translateP proofForm f r
      proofForall' =
        PForallI
          { newVar = x'
          , proofForm = proofForm'
          }
     in
      (proofForall', forall')

translateForallE :: Form -> Proof -> Form -> (Proof, Form)
translateForallE
  formReplace
  PForallE
    { var = x
    , form = f
    , termReplace = t
    , proofForall = proofForall
    }
  r =
    let
      formReplace' = translateF formReplace r
      _forall = FForall x f
      f' = translateF f r
      (proofForall', _) = translateP proofForall _forall r
      proofFormReplace' =
        PForallE
          { var = x
          , form = f'
          , termReplace = t
          , proofForall = proofForall'
          }
     in
      (proofFormReplace', formReplace')

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

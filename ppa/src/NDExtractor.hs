{- TODOs
- Validar que R no tenga FVs (porque sino hay que hacer libre de captura, y no
  debería ser necesario)

-}
module NDExtractor (translateF, translateP, dNegRElim) where

import Debug.Trace (trace)
import ND (Form (..), HypId, Proof (..), Term (TVar), proofName)
import NDProofs (Result, cut, hypAndForm, hypForm)
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
  POrE{} -> translateOrE form proof r
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

translateOrE :: Form -> Proof -> Form -> (Proof, Form)
translateOrE
  form
  POrE
    { left = left
    , right = right
    , proofOr = proofOr
    , hypLeft = hypLeft
    , proofAssumingLeft = proofAssumingLeft
    , hypRight = hypRight
    , proofAssumingRight = proofAssumingRight
    }
  r = case translateP proofOr (FOr left right) r of
    (proofOr', or'@(FImp (FAnd (FImp left' _) (FImp right' _)) _)) ->
      let
        (proofAssumingLeft', _) = translateP proofAssumingLeft left r
        (proofAssumingRight', _) = translateP proofAssumingRight right r

        form' = translateF form r
        (dNegRForm', h_dNegRForm') = hypAndForm (FImp (FImp form' r) r)
        -- ~r~r form'
        proofDNegRForm =
          PImpI
            { hypAntecedent = hypForm (FImp form' r)
            , proofConsequent =
                PImpE
                  { antecedent = form'
                  , proofImp = PAx $ hypForm (FImp form' r)
                  , -- proof r asumiendo ~r form.
                    -- Como sabemos que vale left | right, y su traducción es ~r(~r left~~ & ~r right~~)
                    -- podemos eliminar ese no, y demostrar el and de forma simétrica, asumimos left~~ o right~~
                    -- y llegamos a un absurdo usando la demo que lo asume correspondiente.
                    proofAntecedent =
                      PImpE
                        { antecedent = undefined
                        , proofImp = undefined
                        , proofAntecedent = undefined
                        }
                  }
            }
        proofForm' =
          cut
            dNegRForm'
            proofDNegRForm
            h_dNegRForm'
            (dNegRElim form h_dNegRForm' r)
       in
        (proofForm', form')
    f' -> error ("unexpected format " ++ show f')

{- Demuestra ~r~r A~~ |- A~~ por inducción estructural en A (sin traducir)

Intuición atrás del truquito

por ej. para A & B. Si sabemos que vale
~r~r (A~~ & B~~) y lo queremos eliminar para demostrar, va a demostrar r, pero no
tenemos que demostrar r sino A~~ & B~~.
Por eso usamos el truco de dneg elim (HI) para pasar a demostrar ~r~r A~~,
porque introduciendo, tenemos que demostrar r.
-}
dNegRElim :: Form -> HypId -> Form -> Proof
dNegRElim f h_dneg_f' r = case (f, translateF f r) of
  (FFalse, r2) ->
    PImpE
      { antecedent = FImp r r
      , proofImp = PAx h_dneg_f'
      , proofAntecedent =
          PImpI
            { hypAntecedent = hypForm r
            , proofConsequent = PAx $ hypForm r
            }
      }
  (FTrue, FTrue) -> PTrueI
  -- Todas las que su traducción arranca con un ~r son iguales, porque
  -- alcanza con hacer eliminación de triple negación que vale siempre.

  -- dneg en realidad es qneg, entonces eliminando triple quedan iguales.
  (FPred p ts, FImp (FImp f _) _) -> tNegRElim (FImp f r) h_dneg_f' r
  (FNot g, f'@(FImp g' _)) -> tNegRElim g' h_dneg_f' r
  (FExists x g, FImp (FForall _ (FImp g' _)) _) -> tNegRElim (FForall x (FImp g' r)) h_dneg_f' r
  (FOr left right, FImp or' r) -> tNegRElim or' h_dneg_f' r
  (FAnd left right, f'@(FAnd left' right')) ->
    let
      (dneg_left', h_dneg_left') = hypAndForm $ FImp (FImp left' r) r
      proof_left' = dNegRElim left h_dneg_left' r
      proof_dneg_left' =
        PImpI
          { hypAntecedent = hypForm $ FImp left' r
          , proofConsequent =
              PImpE
                { antecedent = FImp f' r
                , proofImp = PAx h_dneg_f'
                , proofAntecedent =
                    PImpI
                      { hypAntecedent = hypForm f'
                      , -- ~r left~~, left~~ & right~~ |- r
                        proofConsequent =
                          PImpE
                            { antecedent = left'
                            , proofImp = PAx $ hypForm $ FImp left' r
                            , proofAntecedent =
                                PAndE1
                                  { right = right'
                                  , proofAnd = PAx $ hypForm f'
                                  }
                            }
                      }
                }
          }

      (dneg_right', h_dneg_right') = hypAndForm $ FImp (FImp right' r) r
      proof_right' = dNegRElim right h_dneg_right' r
      proof_dneg_right' =
        PImpI
          { hypAntecedent = hypForm $ FImp right' r
          , proofConsequent =
              PImpE
                { antecedent = FImp f' r
                , proofImp = PAx h_dneg_f'
                , proofAntecedent =
                    PImpI
                      { hypAntecedent = hypForm f'
                      , -- ~r right~~, left~~ & right~~ |- r
                        proofConsequent =
                          PImpE
                            { antecedent = right'
                            , proofImp = PAx $ hypForm $ FImp right' r
                            , proofAntecedent =
                                PAndE2
                                  { left = left'
                                  , proofAnd = PAx $ hypForm f'
                                  }
                            }
                      }
                }
          }
     in
      PAndI
        { proofLeft = cut dneg_left' proof_dneg_left' h_dneg_left' proof_left'
        , proofRight = cut dneg_right' proof_dneg_right' h_dneg_right' proof_right'
        }
  (FImp ant cons, f'@(FImp ant' cons')) ->
    let
      (dneg_cons', h_dneg_cons') = hypAndForm $ FImp (FImp cons' r) r
      proof_dneg_elim_cons' = dNegRElim cons h_dneg_cons' r
      proof_dneg_cons' =
        PImpI
          { hypAntecedent = hypForm $ FImp cons' r
          , proofConsequent =
              PImpE
                { antecedent = FImp f' r
                , proofImp = PAx h_dneg_f'
                , proofAntecedent =
                    PImpI
                      { hypAntecedent = hypForm f'
                      , proofConsequent =
                          PImpE
                            { antecedent = cons'
                            , proofImp = PAx $ hypForm $ FImp cons' r
                            , proofAntecedent =
                                PImpE
                                  { antecedent = ant'
                                  , proofImp = PAx $ hypForm f'
                                  , proofAntecedent = PAx $ hypForm ant'
                                  }
                            }
                      }
                }
          }
     in
      PImpI
        { hypAntecedent = hypForm ant'
        , proofConsequent = cut dneg_cons' proof_dneg_cons' h_dneg_cons' proof_dneg_elim_cons'
        }
  (FForall x g, f'@(FForall _ g')) ->
    let
      (dneg_g', h_dneg_g') = hypAndForm $ FImp (FImp g' r) r
      proof_dneg_elim_g' = dNegRElim g h_dneg_g' r
      proof_dneg_g' =
        PImpI
          { hypAntecedent = hypForm $ FImp g' r
          , proofConsequent =
              PImpE
                { antecedent = FImp f' r
                , proofImp = PAx h_dneg_f'
                , proofAntecedent =
                    PImpI
                      { hypAntecedent = hypForm f'
                      , proofConsequent =
                          PImpE
                            { antecedent = g'
                            , proofImp = PAx $ hypForm $ FImp g' r
                            , proofAntecedent =
                                PForallE
                                  { var = x
                                  , form = g'
                                  , termReplace = TVar x
                                  , proofForall = PAx $ hypForm f'
                                  }
                            }
                      }
                }
          }
     in
      PForallI
        { newVar = x
        , proofForm = cut dneg_g' proof_dneg_g' h_dneg_g' proof_dneg_elim_g'
        }
  (_, f') -> error $ printf "dnegRElim: unexpected form '%s', translated: '%s'" (show f) (show f')

-- Demuestra ~r~r~r A |- ~r A
tNegRElim :: Form -> HypId -> Form -> Proof
tNegRElim f h_tneg r =
  PImpI
    { hypAntecedent = hypForm f
    , proofConsequent =
        PImpE
          { antecedent = FImp (FImp f r) r
          , proofImp = PAx h_tneg
          , proofAntecedent =
              PImpI
                { hypAntecedent = hypForm $ FImp f r
                , proofConsequent =
                    PImpE
                      { antecedent = f
                      , proofImp = PAx $ hypForm (FImp f r)
                      , proofAntecedent = PAx $ hypForm f
                      }
                }
          }
    }

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

{- TODOs
- Validar que R no tenga FVs (porque sino hay que hacer libre de captura, y no
  debería ser necesario)

-}
module NDExtractor (
  translateF,
  translateP,
  translateE,
  dNegRElim,
  rElim,
) where

import Control.DeepSeq (force)

-- import Debug.Trace (trace)
import ND (Env (EEmpty, EExtend), Form (..), HypId, Proof (..), Term (TVar), proofName)
import NDProofs (Result, cut, hypAndForm, hypForm)
import NDReducer (reduce)
import NDSubst (subst)
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

-- Traduce f via doble negación relativizada, parametrizada por una fórmula
-- arbitraria R.
-- FNot a ~~> FImp a r (FNot_R)
translateF :: Int -> Form -> Form -> Form
-- translateF idt f r | trace (printf "%s (%d) translateF %s %s" (indent idt) idt (show f) (show r)) False = undefined
translateF idt form r = case form of
  FAnd left right -> FAnd (rec left) (rec right)
  FOr left right -> fNotR (FAnd (fNotR (rec left)) (fNotR (rec right)))
  FImp left right -> FImp (rec left) (rec right)
  FNot g -> fNotR (rec g)
  FForall x g -> FForall x (rec g)
  FExists x g -> fNotR (FForall x (fNotR (rec g)))
  FFalse -> r
  FTrue -> FTrue
  g@(FPred p ts) -> fNotR (fNotR g)
 where
  rec form' = translateF (idt + 1) form' r
  fNotR form' = FImp form' r

-- Necesario para algunos tests que hacen check con un env que no es empty
translateE :: Env -> Form -> Env
translateE env r = case env of
  EEmpty -> EEmpty
  EExtend h f env2 -> EExtend h (translateF 0 f r) (translateE env2 r)

indent :: Int -> String
indent n = concat $ replicate (n * 2) "  "

translateP :: Int -> Proof -> Form -> Form -> (Proof, Form)
translateP = force translateP'

-- Convierte una demostración clásica en una intuicionista usando la traducción de friedman.
translateP' :: Int -> Proof -> Form -> Form -> (Proof, Form)
-- translateP' idt proof form r | trace (printf "%s translateP %s %s %s" (indent idt) (proofName proof) (show form) (show proof)) False = undefined
translateP' idt proof form r = case proof of
  PAx h -> (PAx h, translateF (idt + 1) form r)
  PNamed n p ->
    let (p', f') = translateP (idt + 1) p form r
     in (PNamed n p', f')
  {- Imp -}
  PImpE{} -> translateImpE idt form proof r
  PImpI{} -> translateImpI idt form proof r
  {- And -}
  PAndI{} -> translateAndI idt form proof r
  PAndE1{} -> translateAndE1 idt form proof r
  PAndE2{} -> translateAndE2 idt form proof r
  {- False -}
  PFalseE proofBot ->
    let (proofBot', _) = translateP (idt + 1) proofBot FFalse r
        form' = translateF (idt + 1) form r
        proofForm' = rElim (idt + 1) form proofBot' r
     in (proofForm', form')
  {- True -}
  PTrueI -> (PTrueI, translateF (idt + 1) form r)
  {- LEM -}
  PLEM -> translateLEM idt form proof r
  {- Or -}
  POrI1{} -> translateOrI1 idt form proof r
  POrI2{} -> translateOrI2 idt form proof r
  POrE{} -> translateOrE idt form proof r
  {- Forall -}
  PForallI{} -> translateForallI idt form proof r
  PForallE{} -> translateForallE idt form proof r
  {- Exists -}
  PExistsI{} -> translateExistsI idt form proof r
  PExistsE{} -> translateExistsE idt form proof r
  {- Not -}
  PNotI{} -> translateNotI idt form proof r
  PNotE{} -> translateNotE idt form proof r

translateImpI :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateImpI
  idt
  imp@(FImp ant cons)
  PImpI
    { hypAntecedent = h
    , proofConsequent = proofCons
    }
  r =
    let
      (proofCons', cons'_1) = translateP (idt + 1) proofCons cons r
      imp'@(FImp _ cons'_2) = translateF (idt + 1) imp r
      _ = assertEq cons'_1 cons'_2
      proofImp' =
        PImpI
          { hypAntecedent = h
          , proofConsequent = proofCons'
          }
     in
      (proofImp', imp')

assertEq :: Form -> Form -> Form
assertEq f1 f2
  | f1 /= f2 = error $ printf "expected %s == %s" (show f1) (show f2)
  | otherwise = f1

translateImpE :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateImpE
  idt
  cons
  PImpE
    { antecedent = ant
    , proofImp = proofImp
    , proofAntecedent = proofAnt
    }
  r =
    let
      imp = FImp ant cons
      (proofImp', imp') = translateP (idt + 1) proofImp imp r
      (proofAnt', ant') = translateP (idt + 1) proofAnt ant r
      cons' = translateF (idt + 1) cons r
      _ = assertEq (FImp ant' cons') imp'
      proofCons' =
        PImpE
          { antecedent = ant'
          , proofImp = proofImp'
          , proofAntecedent = proofAnt'
          }
     in
      (proofCons', cons')

translateAndI :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateAndI
  idt
  and@(FAnd left right)
  PAndI
    { proofLeft = proofL
    , proofRight = proofR
    }
  r =
    let
      (proofLeft', left'_1) = translateP (idt + 1) proofL left r
      (proofRight', right'_1) = translateP (idt + 1) proofR right r
      and'@(FAnd left'_2 right'_2) = translateF (idt + 1) and r
      _ = assertEq left'_1 left'_2
      _ = assertEq right'_1 right'_2
      proofAnd' =
        PAndI
          { proofLeft = proofLeft'
          , proofRight = proofRight'
          }
     in
      (proofAnd', and')

translateAndE1 :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateAndE1
  idt
  left
  PAndE1
    { right = right
    , proofAnd = proofAnd
    }
  r =
    let
      and = FAnd left right
      (proofAnd', _) = translateP (idt + 1) proofAnd and r
      left' = translateF (idt + 1) left r
      right' = translateF (idt + 1) right r
      proofL' =
        PAndE1
          { right = right'
          , proofAnd = proofAnd'
          }
     in
      (proofL', left')

translateAndE2 :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateAndE2
  idt
  right
  PAndE2
    { left = left
    , proofAnd = proofAnd
    }
  r =
    let
      and = FAnd left right
      (proofAnd', _) = translateP (idt + 1) proofAnd and r
      left' = translateF (idt + 1) left r
      right' = translateF (idt + 1) right r
      proofR' =
        PAndE2
          { left = left'
          , proofAnd = proofAnd'
          }
     in
      (proofR', right')

translateLEM :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateLEM
  idt
  or@(FOr f (FNot g))
  PLEM
  r =
    -- ~R (~R f~~ & ~R~R f~~)
    case translateF (idt + 1) or r of
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

translateOrI1 :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateOrI1
  idt
  or@(FOr left right)
  POrI1
    { proofLeft = proofLeft
    }
  r =
    -- ~r (~r left~~ & ~r right~~)
    case translateF (idt + 1) or r of
      or'@(FImp and@(FAnd (FImp left' _) notR_right') r) ->
        let
          (proofLeft', _) = translateP (idt + 1) proofLeft left r
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
translateOrI1 _ f p r = error $ printf "translateOrI1: unexpected form '%s' with proof '%s'" (show f) (proofName p)

translateOrI2 :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateOrI2
  idt
  or@(FOr left right)
  POrI2
    { proofRight = proofRight
    }
  r =
    -- ~r (~r left~~ & ~r right~~)
    case translateF (idt + 1) or r of
      or'@(FImp and@(FAnd notR_left' (FImp right' _)) r) ->
        let
          (proofRight', _) = translateP (idt + 1) proofRight right r
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

translateForallI :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateForallI
  idt
  _forall@(FForall x f)
  PForallI
    { newVar = x'
    , proofForm = proofForm
    }
  r =
    let
      -- TODO: Check que la f' de aca y de proofForm' sean iguales
      forall' = translateF (idt + 1) _forall r
      (proofForm', _) = translateP (idt + 1) proofForm f r
      proofForall' =
        PForallI
          { newVar = x'
          , proofForm = proofForm'
          }
     in
      (proofForall', forall')

translateForallE :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateForallE
  idt
  formReplace
  PForallE
    { var = x
    , form = f
    , termReplace = t
    , proofForall = proofForall
    }
  r =
    let
      formReplace' = translateF (idt + 1) formReplace r
      _forall = FForall x f
      f' = translateF (idt + 1) f r
      (proofForall', _) = translateP (idt + 1) proofForall _forall r
      proofFormReplace' =
        PForallE
          { var = x
          , form = f'
          , termReplace = t
          , proofForall = proofForall'
          }
     in
      (proofFormReplace', formReplace')

translateOrE :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateOrE
  idt
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
  r = case translateP (idt + 1) proofOr (FOr left right) r of
    (proofOr', or'@(FImp (FAnd (FImp left' _) (FImp right' _)) _)) ->
      let
        and' = FAnd (FImp left' r) (FImp right' r)
        (proofAssumingLeft', _) = translateP (idt + 1) proofAssumingLeft form r
        (proofAssumingRight', _) = translateP (idt + 1) proofAssumingRight form r

        form' = translateF (idt + 1) form r
        (dnegr_form', h_dnegr_form') = hypAndForm (FImp (FImp form' r) r)
        proof_dnegr_elim_form' = dNegRElim (idt + 1) form h_dnegr_form' r
        -- ~r~r form'
        proof_dnegr_form' =
          PImpI
            { hypAntecedent = hypForm (FImp form' r)
            , {- proof r asumiendo ~r form.
              Como sabemos que vale left | right, y su traducción es ~r(~r left~~ & ~r right~~)
              podemos eliminar ese no, y demostrar el and de forma simétrica,
              asumimos left~~ o right~~ y llegamos a un absurdo usando la demo que
              lo asume correspondiente.
              -}
              -- Acá si o si hay que eliminar ~r(~r left~~ & ~r right~~) en vez de
              -- ~r (form~~) porque es el único lugar en la demo en el que hay que
              -- demostrar r.
              proofConsequent =
                PImpE
                  { antecedent = and'
                  , proofImp = proofOr'
                  , proofAntecedent =
                      PAndI
                        { proofLeft =
                            PImpI
                              { hypAntecedent = hypLeft
                              , proofConsequent =
                                  PImpE
                                    { antecedent = form'
                                    , proofImp = PAx $ hypForm (FImp form' r)
                                    , proofAntecedent = proofAssumingLeft'
                                    }
                              }
                        , proofRight =
                            PImpI
                              { hypAntecedent = hypRight
                              , proofConsequent =
                                  PImpE
                                    { antecedent = form'
                                    , proofImp = PAx $ hypForm (FImp form' r)
                                    , proofAntecedent = proofAssumingRight'
                                    }
                              }
                        }
                  }
            }
        proofForm' =
          cut
            dnegr_form'
            proof_dnegr_form'
            h_dnegr_form'
            proof_dnegr_elim_form'
       in
        (proofForm', form')
    f' -> error ("unexpected format " ++ show f')

translateNotI :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateNotI
  idt
  formNot
  PNotI
    { hyp = h
    , proofBot = proofBot
    }
  r =
    let
      (proofBot', _) = translateP (idt + 1) proofBot FFalse r
      formNot' = translateF (idt + 1) formNot r
      proofNot' =
        PImpI
          { hypAntecedent = h
          , proofConsequent = proofBot'
          }
     in
      (proofNot', formNot')

translateNotE :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateNotE
  idt
  false
  PNotE
    { form = form
    , proofNotForm = proofNotForm
    , proofForm = proofForm
    }
  r =
    let
      (proofNotForm', _) = translateP (idt + 1) proofNotForm (FNot form) r
      (proofForm', form') = translateP (idt + 1) proofForm form r
      false' = translateF (idt + 1) false r
      proofFalse' =
        PImpE
          { antecedent = form'
          , proofImp = proofNotForm'
          , proofAntecedent = proofForm'
          }
     in
      (proofFalse', false')

translateExistsI :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateExistsI
  idt
  formExists@(FExists x g)
  PExistsI
    { inst = t
    , proofFormWithInst = proofFormWithInst
    }
  r = case translateF (idt + 1) formExists r of
    formExists'@(FImp (FForall _ (FImp g' _)) _) ->
      let
        _forall = FForall x (FImp g' r)
        (proofFormWithInst', formWithInst') = translateP (idt + 1) proofFormWithInst (subst x t g) r
        proofExists' =
          PImpI
            { hypAntecedent = hypForm _forall
            , proofConsequent =
                PImpE
                  { antecedent = formWithInst'
                  , proofImp =
                      PForallE
                        { var = x
                        , form = FImp g' r
                        , termReplace = t
                        , proofForall = PAx $ hypForm _forall
                        }
                  , proofAntecedent = proofFormWithInst'
                  }
            }
       in
        (proofExists', formExists')
    f' -> error ("unexpected format " ++ show f')

translateExistsE :: Int -> Form -> Proof -> Form -> (Proof, Form)
translateExistsE
  idt
  form
  PExistsE
    { var = x
    , form = g
    , proofExists = proofExists
    , hyp = h_g
    , proofAssuming = proofAssuming
    }
  r =
    case translateP (idt + 1) proofExists (FExists x g) r of
      (proofExists', exists'@(FImp (FForall _ (FImp g' _)) _)) ->
        let
          _forall = FForall x (FImp g' r)
          (proofAssuming', _) = translateP (idt + 1) proofAssuming form r
          form' = translateF (idt + 1) form r
          (dnegr_form', h_dnegr_form') = hypAndForm (FImp (FImp form' r) r)
          proof_dnegr_elim_form' = dNegRElim (idt + 1) form h_dnegr_form' r
          -- ~r~r form'
          proof_dnegr_form' =
            PImpI
              { hypAntecedent = hypForm $ FImp form' r
              , proofConsequent =
                  PImpE
                    { antecedent = _forall
                    , proofImp = proofExists'
                    , proofAntecedent =
                        PForallI
                          { newVar = x
                          , proofForm =
                              PImpI
                                { hypAntecedent = h_g -- Misma
                                , proofConsequent =
                                    PImpE
                                      { antecedent = form'
                                      , proofImp = PAx $ hypForm $ FImp form' r
                                      , proofAntecedent = proofAssuming'
                                      }
                                }
                          }
                    }
              }

          proofForm' =
            cut
              dnegr_form'
              proof_dnegr_form'
              h_dnegr_form'
              proof_dnegr_elim_form'
         in
          (proofForm', form')
      f' -> error ("unexpected format " ++ show f')

{- Demuestra r |- A~~ por inducción estructural en A.

Asume que la demostración de r ya está traducida.
-}
rElim :: Int -> Form -> Proof -> Form -> Proof
-- rElim idt f _ _ | trace (printf "%s rElim %s" (indent idt) (show f)) False = undefined
rElim idt f proof_r r = case (f, translateF (idt + 1) f r) of
  (FFalse, _) -> proof_r
  (FTrue, FTrue) -> PTrueI
  (FPred p ts, FImp (FImp f _) _) ->
    PImpI
      { hypAntecedent = hypForm $ FImp f r
      , proofConsequent = proof_r
      }
  (FNot g, FImp g' _) ->
    PImpI
      { hypAntecedent = hypForm g'
      , proofConsequent = proof_r
      }
  (FExists x g, FImp _forall _) ->
    PImpI
      { hypAntecedent = hypForm _forall
      , proofConsequent = proof_r
      }
  (FOr left right, FImp or' r) ->
    PImpI
      { hypAntecedent = hypForm or'
      , proofConsequent = proof_r
      }
  (FAnd left right, FAnd left' right') ->
    PAndI
      { proofLeft = rElim (idt + 1) left proof_r r
      , proofRight = rElim (idt + 1) right proof_r r
      }
  (FImp ant cons, FImp ant' cons') ->
    PImpI
      { hypAntecedent = hypForm ant'
      , proofConsequent = rElim (idt + 1) cons proof_r r
      }
  (FForall x g, FForall _ g') ->
    PForallI
      { newVar = x
      , proofForm = rElim (idt + 1) g proof_r r
      }
  (_, f') -> error $ printf "rElim: unexpected form '%s', translated: '%s'" (show f) (show f')

{- Demuestra ~r~r A~~ |- A~~ por inducción estructural en A (sin traducir)

Intuición atrás del truquito

por ej. para A & B. Si sabemos que vale
~r~r (A~~ & B~~) y lo queremos eliminar para demostrar, va a demostrar r, pero no
tenemos que demostrar r sino A~~ & B~~.
Por eso usamos el truco de dneg elim (HI) para pasar a demostrar ~r~r A~~,
porque introduciendo, tenemos que demostrar r.
-}
dNegRElim :: Int -> Form -> HypId -> Form -> Proof
-- dNegRElim idt f h_dneg_f' _ | trace (printf "%s dNegRElim %s" (indent idt) (show f)) False = undefined
dNegRElim idt f h_dneg_f' r = case (f, translateF (idt + 1) f r) of
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
      proof_left' = dNegRElim (idt + 1) left h_dneg_left' r
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
      proof_right' = dNegRElim (idt + 1) right h_dneg_right' r
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
      proof_dneg_elim_cons' = dNegRElim (idt + 1) cons h_dneg_cons' r
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
      proof_dneg_elim_g' = dNegRElim (idt + 1) g h_dneg_g' r
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

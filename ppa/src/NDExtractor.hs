{- TODOs
- Validar que R no tenga FVs (porque sino hay que hacer libre de captura, y no
  debería ser necesario)

-}
module NDExtractor (
  translateF,
  translateP,
  translateE,
  translateFriedman,
  translateContext,
  extractWitnessCtx,
  dNegRElim,
  rElim,
  rIntro,
  transIntro,
) where

import Certifier (checkContext)
import ND (Env (EEmpty, EExtend), Form (..), HypId, Proof (..), Term (TVar), proofName)
import NDChecker (check)
import NDProofs (Result, cut, hypAndForm, hypForm, wrapR)
import NDReducer (reduce)
import NDSubst (subst, substHyp)
import PPA (Context, Hypothesis (HAxiom, HTheorem), axioms, findHyp, getForm, getHypId, removeHyp)
import Text.Printf (printf)

-- TODO: inline proofs
translateContext :: Context -> Form -> Context
translateContext ctx r = map (\h -> translateHyp h r) ctx

translateHyp :: Hypothesis -> Form -> Hypothesis
translateHyp hyp r = case hyp of
  HAxiom h f -> HAxiom h (translateF f r)
  HTheorem h f p -> let (p', f') = translateP p f r in HTheorem h f' p'

{-
Para mantener los axiomas originales, reemplazo ax: f las ocurrencias de ax por
una dem de f~~ a partir de f
-}
extractWitnessCtx :: Context -> HypId -> Result (Context, Term)
extractWitnessCtx ctx theoremId = do
  h_theorem <- findHyp ctx theoremId
  case h_theorem of
    HAxiom{} -> Left $ printf "%s is an axiom, not a theorem" theoremId
    HTheorem h f p -> do
      let r = f
      let ctxRest = removeHyp ctx theoremId
      let pInlined = inlineProofs ctxRest p
      checkContext (axioms ctx ++ [HTheorem h f pInlined])
      let translatedAxioms = translateContext (axioms ctx) r
      (pInlinedTranslated, t) <- extractWitness translatedAxioms pInlined f
      let ctxResult = translatedAxioms ++ [HTheorem h f pInlinedTranslated]
      return (ctxResult, t)

-- Inlinea todos los proofs del context en el proof
inlineProofs :: Context -> Proof -> Proof
inlineProofs ctx p = foldr inlineHyp p ctx
 where
  inlineHyp hyp proof = case hyp of
    HAxiom{} -> proof
    HTheorem h f p_f -> substHyp h p_f proof

{- Dada una demostración de exists X . p(X) devuelve t tal que p(t)
Para ello,
- (wip) traduce la demostración de clásica a intuicionista usando la traducción de Friedman
- reduce la demostración
- si el resultado es una
La fórmula debe ser de la clase Sigma^0_1, es decir N existenciales seguidos de una fórmula sin cuantificadores.
-}
extractWitness :: Context -> Proof -> Form -> Result (Proof, Term)
extractWitness ctxAxioms proof f_exists@(FExists x f) = do
  let proofNJ = translateFriedman proof f_exists
  checkContext (ctxAxioms ++ [HTheorem "h" f_exists proofNJ])
  let reducedProof = reduce proofNJ
  return (reducedProof, TVar "----")
-- case reducedProof of
--   (PExistsI t _) -> Right (reducedProof, t)
--   proof' -> Left "proof not exists introduction, can't extract witness"
extractWitness _ _ f = Left $ printf "form %s must be exists" (show f)

-- Dada una demostración en lógica clásica de una fórmula F, usa el truco de la
-- traducción de Friedman para dar una demostración en lógica intuicionista.
-- TODO: Bancar foralls
translateFriedman :: Proof -> Form -> Proof
translateFriedman p f_exists@(FExists x f) = do
  let r = f_exists
  let (p', f_exists'@(FImp _forall@(FForall _ g) _)) = translateP p f_exists r
  -- p' es una demostración de ~r forall x. ~r f~~, por lo que podemos usarla
  -- para demostrar r (el exists, la form original) si demostramos forall x. ~r f~~
  PImpE
    { antecedent = _forall
    , proofImp = p'
    , proofAntecedent =
        PForallI
          { newVar = x
          , proofForm =
              cut
                (FImp f r)
                ( PImpI
                    { hypAntecedent = hypForm f
                    , proofConsequent =
                        PExistsI
                          { inst = TVar x
                          , proofFormWithInst = PAx $ hypForm f
                          }
                    }
                )
                (hypForm (FImp f r))
                -- ~r A |- ~r A~~
                (rIntro f (hypForm (FImp f r)) r)
          }
    }

-- Demuestra ~r A |- ~r A~~ por inducción estructural en A
rIntro :: Form -> HypId -> Form -> Proof
rIntro f h_notr_f r = case (f, translateF f f) of
  -- ~
  (FFalse, _) -> PAx h_notr_f
  (FTrue, FTrue) -> undefined
  -- ~r A |- ~r ~r ~r A
  (FPred{}, f'@(FImp (FImp f _) _)) ->
    PImpI
      { hypAntecedent = hypForm f'
      , proofConsequent =
          PImpE
            { antecedent = FImp f r
            , proofImp = PAx (hypForm f')
            , proofAntecedent = PAx h_notr_f
            }
      }
  (FNot g, FImp g' _) -> undefined
  (FExists x g, FImp _forall _) -> undefined
  (FOr left right, FImp or' r) -> undefined
  (FAnd left right, FAnd left' right') -> undefined
  (FImp ant cons, FImp ant' cons') -> undefined
  (FForall x g, FForall _ g') -> undefined
  (_, f') -> error $ printf "rIntro: unexpected form '%s', translated: '%s'" (show f) (show f')

{- Demuestra A |- A~~ por inducción estructural en A -}
transIntro :: Form -> HypId -> Form -> Proof
transIntro f h_f r = case (f, translateF f r) of
  (FFalse, _) -> PFalseE (PAx h_f)
  (FTrue, FTrue) -> PTrueI
  (FPred{}, FImp notr_f@(FImp f _) _) ->
    PImpI
      { hypAntecedent = hypForm notr_f
      , proofConsequent =
          PImpE
            { antecedent = f
            , proofImp = PAx $ hypForm notr_f
            , proofAntecedent = PAx h_f
            }
      }
  (FNot g, FImp g' _) -> undefined
  -- PImpI
  --   { hypAntecedent = hypForm g
  --   , proofConsequent =
  --       PFalseE $
  --         PNotE
  --           { form = g
  --           , proofNotForm = PAx h_f
  --           , proofForm = PAx $ hypForm g
  --           }
  --   }
  (FExists x g, FImp _forall@(FForall _ (FImp g' _)) _) ->
    let
      h_g = hypForm g
      proofGThenG' = transIntro g h_g r
     in
      PImpI
        { hypAntecedent = hypForm _forall
        , proofConsequent =
            PImpE
              { antecedent = g'
              , proofImp =
                  PForallE
                    { var = x
                    , form = FImp g' r
                    , termReplace = TVar x
                    , proofForall = PAx $ hypForm _forall
                    }
              , -- proof g' sabiendo que vale exists x g, uso HI
                proofAntecedent =
                  cut
                    g
                    ( PExistsE
                        { var = x
                        , form = g
                        , proofExists = PAx h_f
                        , hyp = h_g
                        , proofAssuming = PAx h_g
                        }
                    )
                    h_g
                    proofGThenG'
              }
        }
  (FOr left right, FImp or' r) -> undefined
  (FAnd left right, FAnd left' right') ->
    let (h_left, h_right) = (hypForm left, hypForm right)
        proofLeftThenLeft' = transIntro left h_left r
        proofRightThenRight' = transIntro right h_right r
     in PAndI
          { proofLeft =
              cut
                left
                ( PAndE1
                    { right = right
                    , proofAnd = PAx h_f
                    }
                )
                h_left
                proofLeftThenLeft'
          , proofRight =
              cut
                right
                ( PAndE2
                    { left = left
                    , proofAnd = PAx h_f
                    }
                )
                h_right
                proofRightThenRight'
          }
  (FImp ant cons, FImp ant' cons') ->
    let (h_ant, h_cons) = (hypForm ant, hypForm cons)
        proofAntThenAnt' = transIntro ant h_ant r
        proofConsThenCons' = transIntro cons h_cons r
     in PImpI
          { hypAntecedent = hypForm ant'
          , proofConsequent = cut cons
          }
  (FForall x g, FForall _ g') ->
    let
      h_g = hypForm g
      proofGThenG' = transIntro g h_g r
     in
      PForallI
        { newVar = x
        , proofForm =
            cut
              g
              ( PForallE
                  { var = x
                  , form = g
                  , termReplace = TVar x
                  , proofForall = PAx h_f
                  }
              )
              h_g
              proofGThenG'
        }
  (_, f') -> error $ printf "rIntro: unexpected form '%s', translated: '%s'" (show f) (show f')

-- Traduce f via doble negación relativizada, parametrizada por una fórmula
-- arbitraria R.
-- FNot a ~~> FImp a r (FNot_R)
translateF :: Form -> Form -> Form
translateF form r = case form of
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
  rec form' = translateF form' r
  fNotR form' = FImp form' r

-- Necesario para algunos tests que hacen check con un env que no es empty
translateE :: Env -> Form -> Env
translateE env r = case env of
  EEmpty -> EEmpty
  EExtend h f env2 -> EExtend h (translateF f r) (translateE env2 r)

-- Convierte una demostración clásica en una intuicionista usando la traducción de friedman.
translateP :: Proof -> Form -> Form -> (Proof, Form)
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
  PFalseE proofBot ->
    let (proofBot', _) = translateP proofBot FFalse r
        form' = translateF form r
        proofForm' = rElim form proofBot' r
     in (proofForm', form')
  {- True -}
  PTrueI -> (PTrueI, translateF form r)
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
  PExistsI{} -> translateExistsI form proof r
  PExistsE{} -> translateExistsE form proof r
  {- Not -}
  PNotI{} -> translateNotI form proof r
  PNotE{} -> translateNotE form proof r

translateImpI :: Form -> Proof -> Form -> (Proof, Form)
translateImpI
  imp@(FImp ant cons)
  PImpI
    { hypAntecedent = h
    , proofConsequent = proofCons
    }
  r =
    let
      (proofCons', cons'_1) = translateP proofCons cons r
      imp'@(FImp _ cons'_2) = translateF imp r
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
      (proofImp', imp') = translateP proofImp imp r
      (proofAnt', ant') = translateP proofAnt ant r
      cons' = translateF cons r
      _ = assertEq (FImp ant' cons') imp'
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
      (proofLeft', left'_1) = translateP proofL left r
      (proofRight', right'_1) = translateP proofR right r
      and'@(FAnd left'_2 right'_2) = translateF and r
      _ = assertEq left'_1 left'_2
      _ = assertEq right'_1 right'_2
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
translateLEM
  or@(FOr f (FNot g))
  PLEM
  r =
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
translateOrI1 f p r = error $ printf "translateOrI1: unexpected form '%s' with proof '%s'" (show f) (proofName p)

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
translateOrI2 f p _ = error $ printf "translateOrI2: unexpected form '%s' with proof '%s'" (show f) (proofName p)

translateForallI :: Form -> Proof -> Form -> (Proof, Form)
translateForallI
  _forall@(FForall x f)
  PForallI
    { newVar = x'
    , proofForm = proofForm
    }
  r =
    let
      -- TODO: Check que la f' de aca y de proofForm' sean iguales
      forall' = translateF _forall r
      (proofForm', _) = translateP proofForm f r
      proofForall' =
        PForallI
          { newVar = x'
          , proofForm = proofForm'
          }
     in
      (proofForall', forall')
translateForallI f p _ = error $ printf "translateForallI: unexpected form '%s' with proof '%s'" (show f) (proofName p)

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
translateForallE f p _ = error $ printf "translateForallE: unexpected form '%s' with proof '%s'" (show f) (proofName p)

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
        and' = FAnd (FImp left' r) (FImp right' r)
        (proofAssumingLeft', _) = translateP proofAssumingLeft form r
        (proofAssumingRight', _) = translateP proofAssumingRight form r

        form' = translateF form r
        (dnegr_form', h_dnegr_form') = hypAndForm (FImp (FImp form' r) r)
        proof_dnegr_elim_form' = dNegRElim form h_dnegr_form' r
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

translateNotI :: Form -> Proof -> Form -> (Proof, Form)
translateNotI
  formNot
  PNotI
    { hyp = h
    , proofBot = proofBot
    }
  r =
    let
      (proofBot', _) = translateP proofBot FFalse r
      formNot' = translateF formNot r
      proofNot' =
        PImpI
          { hypAntecedent = h
          , proofConsequent = proofBot'
          }
     in
      (proofNot', formNot')

translateNotE :: Form -> Proof -> Form -> (Proof, Form)
translateNotE
  false
  PNotE
    { form = form
    , proofNotForm = proofNotForm
    , proofForm = proofForm
    }
  r =
    let
      (proofNotForm', _) = translateP proofNotForm (FNot form) r
      (proofForm', form') = translateP proofForm form r
      false' = translateF false r
      proofFalse' =
        PImpE
          { antecedent = form'
          , proofImp = proofNotForm'
          , proofAntecedent = proofForm'
          }
     in
      (proofFalse', false')

translateExistsI :: Form -> Proof -> Form -> (Proof, Form)
translateExistsI
  formExists@(FExists x g)
  PExistsI
    { inst = t
    , proofFormWithInst = proofFormWithInst
    }
  r = case translateF formExists r of
    formExists'@(FImp (FForall _ (FImp g' _)) _) ->
      let
        _forall = FForall x (FImp g' r)
        (proofFormWithInst', formWithInst') = translateP proofFormWithInst (subst x t g) r
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

translateExistsE :: Form -> Proof -> Form -> (Proof, Form)
translateExistsE
  form
  PExistsE
    { var = x
    , form = g
    , proofExists = proofExists
    , hyp = h_g
    , proofAssuming = proofAssuming
    }
  r =
    case translateP proofExists (FExists x g) r of
      (proofExists', exists'@(FImp (FForall _ (FImp g' _)) _)) ->
        let
          _forall = FForall x (FImp g' r)
          (proofAssuming', _) = translateP proofAssuming form r
          form' = translateF form r
          (dnegr_form', h_dnegr_form') = hypAndForm (FImp (FImp form' r) r)
          proof_dnegr_elim_form' = dNegRElim form h_dnegr_form' r
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
rElim :: Form -> Proof -> Form -> Proof
rElim f proof_r r = case (f, translateF f r) of
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
      { proofLeft = rElim left proof_r r
      , proofRight = rElim right proof_r r
      }
  (FImp ant cons, FImp ant' cons') ->
    PImpI
      { hypAntecedent = hypForm ant'
      , proofConsequent = rElim cons proof_r r
      }
  (FForall x g, FForall _ g') ->
    PForallI
      { newVar = x
      , proofForm = rElim g proof_r r
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

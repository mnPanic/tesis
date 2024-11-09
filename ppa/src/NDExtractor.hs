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
  fromPi02,
  toPi02,
  dNegRElim,
  rElim,
  rIntro,
  transIntro,
  inlineAxioms,
) where

import Certifier (checkContext)
import ND (Env (EEmpty, EExtend), Form (..), HypId, Proof (..), Term (TVar), VarId, fvP, proofName)
import NDProofs (Result, cut, hypAndForm, hypForm, wrapR)
import NDReducer (reduce)
import NDSubst (freshWRT, subst, substHyp)
import PPA (Context, Hypothesis (HAxiom, HTheorem), axioms, findHyp, removeHyp)
import Text.Printf (printf)

import Data.Set qualified as Set

-- Para mas declaratividad en los tipos
type R = Form

-- Usado solo para tests
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
extractWitnessCtx :: Context -> HypId -> [Term] -> Result (Context, Term, Form)
extractWitnessCtx ctx theoremId terms = do
  h_theorem <- findHyp ctx theoremId
  case h_theorem of
    HAxiom{} -> Left $ printf "%s is an axiom, not a theorem" theoremId
    HTheorem h f p -> do
      let ctxRest = removeHyp ctx theoremId
      let pInlined = inlineProofs ctxRest p
      wrapR "check inlined" $ checkContext (axioms ctx ++ [HTheorem h f pInlined])

      (pInlinedTranslated, f_exists, t) <- extractWitness (axioms ctx) pInlined f terms
      let (FExists x f') = f_exists

      let ctxResult = axioms ctx ++ [HTheorem h f_exists pInlinedTranslated]

      let f_witnessed = subst x t f'
      return (ctxResult, t, f_witnessed)

-- Inlinea todos los proofs del context en el proof
inlineProofs :: Context -> Proof -> Proof
inlineProofs ctx p = foldr inlineHyp p ctx
 where
  inlineHyp hyp proof = case hyp of
    HAxiom{} -> proof
    HTheorem h _ p_f -> substHyp h (PNamed (printf "theorem %s" h) p_f) proof

inlineAxioms :: Context -> Proof -> R -> Proof
inlineAxioms ctx p r = foldr inlineAxiom p ctx
 where
  inlineAxiom hyp proof = case hyp of
    HTheorem{} -> proof
    HAxiom h f ->
      substHyp
        h
        ( PNamed
            (printf "axiom %s translation intro" h)
            (transIntro f h r)
        )
        proof

{- Dada una demostración de forall Y. exists X . p(Y, X), dada una instancia de Y:=a,
 devuelve t tal que p(a, t).

Para ello,

- traduce la demostración de clásica a intuicionista usando la traducción de
  Friedman (da una demostración intuicionista de forall Y . exists X . p(X))
- inlinea traducción de los axiomas
- usa la demostración para demostrar el goal instanciando los foralls (exists X . p(a, X))
- chequea la demostración resultante
- reduce la demostración
- en la forma normal, debería comenzar con un PExistsI

La fórmula debe ser de la clase Sigma^0_1, es decir N
existenciales seguidos de una fórmula sin cuantificadores.
-}
extractWitness :: Context -> Proof -> Form -> [Term] -> Result (Proof, Form, Term)
extractWitness ctxAxioms proof form instanceTerms = do
  formPi02@(ys, f_exists) <- toPi02 form
  if length ys /= length instanceTerms
    then Left $ printf "need to instantiate forall vars: '%s', but got different number of terms to instantiate: '%s'" (show ys) (show instanceTerms)
    else do
      let (proofNJ, r) = translateFriedman proof formPi02

      -- proofNJ asume que los axiomas están traducidos. Para que se mantengan,
      -- demostramos que los axiomas originales implican su traducción.
      let proofNJAdaptedAxioms = inlineAxioms ctxAxioms proofNJ r

      -- Queremos demostrar la fórmula con los foralls instanciados, para que
      -- la fórmula final sea un exists y la demostración normalizada arranque
      -- con PExistsI.
      -- Como proofNJ es una dem de la fórmula original, con los foralls, la
      -- usamos para demostrar solo el exists.
      let f_exists_inst = foldr (\(y, t) f -> subst y t f) f_exists (zip ys instanceTerms)
      let (proofInst, _) =
            foldr
              ( \t (subProof, (x : xs, subForm)) ->
                  ( PForallE
                      { var = x
                      , form = fromPi02 (xs, subForm)
                      , termReplace = t
                      , proofForall = subProof
                      }
                  , (xs, subst x t subForm)
                  )
              )
              (proofNJAdaptedAxioms, formPi02)
              (reverse instanceTerms)
      wrapR "check pre-reduce" $ checkContext (ctxAxioms ++ [HTheorem "h" f_exists_inst proofInst])

      let reducedProof = reduce proofInst
      case reducedProof of
        (PExistsI t _) -> return (reducedProof, f_exists_inst, t)
        proof' -> return (reducedProof, f_exists_inst, TVar $ printf "(broken) %s not ExistsI" (proofName proof'))

{- Dada una demostración en lógica clásica de una fórmula F, usa el truco de la
traducción de Friedman para dar una demostración en lógica intuicionista de la
misma fórmula.

 |-_C F ~~~> |-_I F

-}
translateFriedman :: Proof -> FormPi02 -> (Proof, R)
translateFriedman proof form@(ys, f_exists@(FExists x _)) = do
  let proofVars = fvP proof
  let ys' = foldr (\y rest -> freshWRT y (Set.union proofVars (Set.fromList rest)) : rest) [] ys
  let f_exists_fresh@(FExists _ f') = foldr (\(y, y') f_exists' -> subst y (TVar y') f_exists') f_exists (zip ys ys')

  let r = f_exists_fresh

  let (proof', form') = translateP proof (fromPi02 form) r
  let split_form'@(_, FImp (FForall _ _) _) = splitForalls form'

  let (FImp forall_fresh@(FForall _ _) _) = translateF f_exists_fresh r

  let (proofExists', _) =
        foldr
          ( \t (subProof, (z : zs, subForm)) ->
              ( PForallE
                  { var = z
                  , form = fromPi02 (zs, subForm)
                  , termReplace = t
                  , proofForall = subProof
                  }
              , (zs, subst z t subForm)
              )
          )
          (proof', split_form')
          (reverse $ map TVar ys')

  let proofExists =
        PImpE
          { antecedent = forall_fresh
          , proofImp = PNamed "proofExists inner" proofExists'
          , proofAntecedent =
              PForallI
                { newVar = x
                , proofForm =
                    cut
                      (FImp f' r)
                      ( PImpI
                          { hypAntecedent = hypForm f'
                          , proofConsequent =
                              PExistsI
                                { inst = TVar x
                                , proofFormWithInst = PAx $ hypForm f'
                                }
                          }
                      )
                      (hypForm (FImp f' r))
                      -- ~r A |- ~r A~~
                      (rIntro f' (hypForm (FImp f' r)) r)
                }
          }

  ( foldr
      ( \y subProof ->
          PForallI
            { newVar = y
            , proofForm = subProof
            }
      )
      proofExists
      ys'
    , r
    )

-- ([y_1, ..., y_n], exists x f) --> forall y_1 ... forall y_n . exists x. f
type FormPi02 = ([VarId], Form)

-- Interpreta una fórmula como PI^0_2, i.e
--   forall y_1 . forall y_2 ... forall y_n . exists x . f
toPi02 :: Form -> Result FormPi02
toPi02 f = case splitForalls f of
  r@(_, FExists{}) -> return r
  (_, f') -> Left $ printf "pi02: %s is not exists or forall" (show f')

-- forall y_1 . forall y_2 ... forall y_n . f -> ([y_1, ..., y_n], f)
splitForalls :: Form -> FormPi02
splitForalls (FForall y f) =
  let (xs, f') = splitForalls f
   in (y : xs, f')
splitForalls f = ([], f)

fromPi02 :: FormPi02 -> Form
fromPi02 (vs, f) = foldr FForall f vs

-- Demuestra ~r A |- ~r A~~ por inducción estructural en A
--
-- Como no vale siempre A |- A~~ (ver Peter Selinger), y para los casos de not e
-- imp en transIntro se usa este lema, no puede valer siempre tampoco.
rIntro :: Form -> HypId -> R -> Proof
rIntro f h_notr_f r = case (f, translateF f r) of
  (FFalse, _) ->
    PImpI
      { hypAntecedent = hypForm r
      , proofConsequent = PAx $ hypForm r
      }
  (FTrue, FTrue) -> PAx h_notr_f
  -- ~r A |- ~r ~r ~r A
  (FPred{}, f'@(FImp (FImp _ _) _)) ->
    PImpI
      { hypAntecedent = hypForm f'
      , proofConsequent =
          PImpE
            { antecedent = FImp f r
            , proofImp = PAx (hypForm f')
            , proofAntecedent = PAx h_notr_f
            }
      }
  (FAnd left right, and'@(FAnd left' right')) -> do
    -- HIs
    let h_notR_left = hypForm (FImp left r)
    let notR_left_then_notR_left' = rIntro left h_notR_left r

    let h_notR_right = hypForm (FImp right r)
    let notR_right_then_notR_right' = rIntro right h_notR_right r

    -- Sub-dem de ~r (a & b) |- ~r ~r (~r a~~ v ~r b~~), igual a la clásica,
    -- ver NDProofs.proofNotDistOverAnd
    let fOr = FOr (FImp left' r) (FImp right' r)
    let proofNotRDistOverAnd =
          PNamed "proofNotRDistOverAnd" $
            PImpI
              { hypAntecedent = hypForm (FImp fOr r)
              , proofConsequent =
                  PImpE
                    { antecedent = fOr
                    , proofImp = PAx (hypForm (FImp fOr r))
                    , proofAntecedent =
                        POrI1
                          { -- ~r a~~, vamos a usar la HI para llevarlo a ~r a
                            proofLeft =
                              cut
                                (FImp left r)
                                ( PImpI
                                    { hypAntecedent = hypForm left
                                    , -- Ya tenemos left como hyp, ahora damos la
                                      -- vuelta, de vuelta eliminamos lo mismo, y
                                      -- vamos a tener right para abs con ~r (a & b)
                                      proofConsequent =
                                        PImpE
                                          { antecedent = fOr
                                          , proofImp = PAx (hypForm (FImp fOr r))
                                          , proofAntecedent =
                                              POrI2
                                                { -- Idem, ~r b~~ lo llevamos a ~r b
                                                  proofRight =
                                                    cut
                                                      (FImp right r)
                                                      ( PImpI
                                                          { hypAntecedent = hypForm right
                                                          , proofConsequent =
                                                              PImpE
                                                                { antecedent = FAnd left right
                                                                , proofImp = PAx h_notr_f
                                                                , proofAntecedent =
                                                                    PAndI
                                                                      { proofLeft = PAx (hypForm left)
                                                                      , proofRight = PAx (hypForm right)
                                                                      }
                                                                }
                                                          }
                                                      )
                                                      h_notR_right
                                                      notR_right_then_notR_right'
                                                }
                                          }
                                    }
                                )
                                h_notR_left
                                notR_left_then_notR_left'
                          }
                    }
              }

    -- Dem de ~r (a & b) |- ~r (a & b)~~
    -- Usamos ~r ~r ~r (a & b)~~, demostrado con tnegr elim, para poder razonar
    -- por el absurdo
    let tNegrAnd' = FImp (FImp (FImp and' r) r) r
    let dNegOr = FImp (FImp fOr r) r
    cut
      tNegrAnd'
      -- Dem de ~r ~r ~r (a & b)~~ usando que ~r (a & b) === ~r a~~ v ~r b~~
      ( cut
          dNegOr
          -- Dem de ~r (a & b) |- ~r ~r (~r a~~ v ~r b~~)
          proofNotRDistOverAnd
          (hypForm dNegOr)
          -- ~r ~r [ ~r a~~ v ~r b~~ ] |- ~r ~r [ ~r (a & b)~~ ]
          -- Usando DNegRCong (no es contravariante)
          ( proofDNegRCong
              fOr
              (FImp and' r)
              (hypForm dNegOr)
              (hypForm fOr)
              (proofNotRDistOverAndRL left' right' (hypForm fOr) r)
              r
          )
      )
      (hypForm tNegrAnd')
      (tNegRElim and' (hypForm tNegrAnd') r)

  -- (FNot g, FImp g' _) -> undefined
  -- (FExists x g, FImp _forall _) -> undefined
  -- (FOr left right, FImp or' r) -> undefined
  -- (FImp ant cons, FImp ant' cons') -> undefined
  -- (FForall x g, FForall{}) -> undefined
  (_, f') -> error $ printf "rIntro: unexpected form '%s', translated: '%s'" (show f) (show f')

-- Dada una demostración de A' |- A, da una de ~r~r A' |- ~r~r A
-- A diferencia de la congruencia de un not normal, esta no se invierte porque
-- es doble.
proofDNegRCong :: Form -> Form -> HypId -> HypId -> Proof -> R -> Proof
proofDNegRCong a' a h_dnegr_a' h_a' proofA'ThenA r =
  PNamed "proofDNegRCong" $
    PImpI
      { hypAntecedent = hypForm (FImp a r)
      , proofConsequent =
          PImpE
            { antecedent = FImp a' r
            , proofImp = PAx h_dnegr_a'
            , proofAntecedent =
                PImpI
                  { hypAntecedent = h_a'
                  , proofConsequent =
                      PImpE
                        { antecedent = a
                        , proofImp = PAx (hypForm (FImp a r))
                        , proofAntecedent = proofA'ThenA
                        }
                  }
            }
      }

-- ~r a v ~r b |- ~r (a & b)
proofNotRDistOverAndRL :: Form -> Form -> HypId -> R -> Proof
proofNotRDistOverAndRL a b hyp_or r =
  PNamed "proofNotRDistOverAndRL" $
    PImpI
      { hypAntecedent = hypForm (FAnd a b)
      , proofConsequent =
          POrE
            { left = FImp a r
            , right = FImp b r
            , proofOr = PAx hyp_or
            , hypLeft = hypForm (FImp a r)
            , proofAssumingLeft =
                PImpE
                  { antecedent = a
                  , proofImp = PAx (hypForm (FImp a r))
                  , proofAntecedent =
                      PAndE1
                        { right = b
                        , proofAnd = PAx (hypForm (FAnd a b))
                        }
                  }
            , hypRight = hypForm (FImp b r)
            , proofAssumingRight =
                PImpE
                  { antecedent = b
                  , proofImp = PAx (hypForm (FImp b r))
                  , proofAntecedent =
                      PAndE2
                        { left = a
                        , proofAnd = PAx (hypForm (FAnd a b))
                        }
                  }
            }
      }

{- Demuestra A |- A~~ por inducción estructural en A -}
transIntro :: Form -> HypId -> R -> Proof
transIntro f h_f r = case (f, translateF f r) of
  (FFalse, _) -> PFalseE (PAx h_f)
  (FTrue, FTrue) -> PTrueI
  (FPred{}, FImp notr_f@(FImp _ _) _) ->
    PImpI
      { hypAntecedent = hypForm notr_f
      , proofConsequent =
          PImpE
            { antecedent = f
            , proofImp = PAx $ hypForm notr_f
            , proofAntecedent = PAx h_f
            }
      }
  (FNot g, FImp{}) ->
    cut
      (FImp g r)
      ( PImpI
          { hypAntecedent = hypForm g
          , proofConsequent =
              PFalseE
                { proofBot =
                    PNotE
                      { form = g
                      , proofNotForm = PAx h_f
                      , proofForm = PAx $ hypForm g
                      }
                }
          }
      )
      (hypForm $ FImp g r)
      (rIntro g (hypForm $ FImp g r) r)
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
  (FOr left right, FImp _and@(FAnd (FImp left' _) (FImp right' _)) _) ->
    let (h_left, h_right) = (hypForm left, hypForm right)
        proofLeftThenLeft' = transIntro left h_left r
        proofRightThenRight' = transIntro right h_right r
     in PImpI
          { hypAntecedent = hypForm _and
          , proofConsequent =
              POrE
                { left = left
                , right = right
                , proofOr = PAx h_f
                , hypLeft = h_left
                , proofAssumingLeft =
                    PImpE
                      { antecedent = left'
                      , proofImp =
                          PAndE1
                            { right = FImp right' r
                            , proofAnd = PAx $ hypForm _and
                            }
                      , proofAntecedent = proofLeftThenLeft'
                      }
                , hypRight = h_right
                , proofAssumingRight =
                    PImpE
                      { antecedent = right'
                      , proofImp =
                          PAndE2
                            { left = FImp left' r
                            , proofAnd = PAx $ hypForm _and
                            }
                      , proofAntecedent = proofRightThenRight'
                      }
                }
          }
  (FAnd left right, FAnd{}) ->
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
  -- Lo transformo al equivalente por el contra-recíproco mediante dos cuts
  -- ~r b -> ~r a |- ~r b~~ -> ~r b~~
  (FImp ant cons, FImp ant' cons') ->
    let
      proofConsThenCons' = transIntro cons (hypForm cons) r
      -- a -> b |- ~r b -> ~r a
      proofContraRepRL =
        PNamed
          "proofContraRepRL"
          PImpI
            { hypAntecedent = hypForm (FImp cons r)
            , proofConsequent =
                PImpI
                  { hypAntecedent = hypForm ant
                  , proofConsequent =
                      PImpE
                        { antecedent = cons
                        , proofImp = PAx $ hypForm (FImp cons r)
                        , proofAntecedent =
                            PImpE
                              { antecedent = ant
                              , proofImp = PAx h_f
                              , proofAntecedent = PAx $ hypForm ant
                              }
                        }
                  }
            }
      -- ~r b~~ -> ~r a~~ |- a~~ -> b~~
      proofContraRepLR =
        PNamed
          "proofContraRepLR"
          PImpI
            { hypAntecedent = hypForm ant'
            , proofConsequent =
                -- Por el absurdo: dneg elim sobre b~~
                cut
                  (FImp (FImp cons' r) r)
                  ( PNamed
                      "proof b~~"
                      PImpI
                        { hypAntecedent = hypForm (FImp cons' r)
                        , proofConsequent =
                            PImpE
                              { antecedent = ant'
                              , proofImp =
                                  PImpE
                                    { antecedent = FImp cons' r
                                    , proofImp = PAx (hypForm (FImp (FImp cons' r) (FImp ant' r)))
                                    , proofAntecedent = PAx (hypForm (FImp cons' r))
                                    }
                              , proofAntecedent = PAx (hypForm ant')
                              }
                        }
                  )
                  (hypForm (FImp (FImp cons' r) r))
                  (PNamed "proof ~r~r b~~ |- b~~" $ dNegRElim cons (hypForm (FImp (FImp cons' r) r)) r)
            }
      --  ~r b -> ~r a |-  ~r b~~ -> ~r a~~. Demo real
      proofTransIntro =
        PNamed
          "proofTransIntro"
          PImpI
            { hypAntecedent = hypForm (FImp cons' r)
            , -- ~r a~~
              proofConsequent =
                cut
                  (FImp ant r)
                  -- ~r a
                  ( PImpE
                      { antecedent = FImp cons r
                      , proofImp = PAx (hypForm (FImp (FImp cons r) (FImp ant r)))
                      , proofAntecedent =
                          -- ~r b~~ |- ~r b
                          PImpI
                            { hypAntecedent = hypForm cons
                            , proofConsequent =
                                PImpE
                                  { antecedent = cons'
                                  , proofImp = PAx $ hypForm (FImp cons' r)
                                  , proofAntecedent = proofConsThenCons'
                                  }
                            }
                      }
                  )
                  (hypForm (FImp ant r))
                  (rIntro ant (hypForm (FImp ant r)) r)
            }
     in
      -- a -> b |- a~~ -> b~~
      cut
        (FImp (FImp cons r) (FImp ant r))
        -- a -> b |- ~r b -> ~r a
        proofContraRepRL
        (hypForm (FImp (FImp cons r) (FImp ant r)))
        ( cut
            (FImp (FImp cons' r) (FImp ant' r))
            --  ~r b -> ~r a |-  ~r b~~ -> ~r a~~. Demo real
            proofTransIntro
            (hypForm (FImp (FImp cons' r) (FImp ant' r)))
            -- ~r b~~ -> ~r a~~ |- a~~ -> b~~
            proofContraRepLR
        )
  -- let (h_ant, h_cons) = (hypForm ant, hypForm cons)
  --     proofAntThenAnt' = transIntro ant h_ant r
  --     proofConsThenCons' = transIntro cons h_cons r
  --  in PImpI
  --       { hypAntecedent = hypForm ant'
  --       , proofConsequent = cut cons
  --       }
  (FForall x g, FForall _ _) ->
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
translateF :: Form -> R -> Form
translateF form r = case form of
  FAnd left right -> FAnd (rec left) (rec right)
  FOr left right -> fNotR (FAnd (fNotR (rec left)) (fNotR (rec right)))
  FImp left right -> FImp (rec left) (rec right)
  FNot g -> fNotR (rec g)
  FForall x g -> FForall x (rec g)
  FExists x g -> fNotR (FForall x (fNotR (rec g)))
  FFalse -> r
  FTrue -> FTrue
  g@(FPred{}) -> fNotR (fNotR g)
 where
  rec form' = translateF form' r
  fNotR form' = FImp form' r

-- Necesario para algunos tests que hacen check con un env que no es empty
translateE :: Env -> R -> Env
translateE env r = case env of
  EEmpty -> EEmpty
  EExtend h f env2 -> EExtend h (translateF f r) (translateE env2 r)

-- Convierte una demostración clásica en una intuicionista usando la traducción de friedman.
translateP :: Proof -> Form -> R -> (Proof, Form)
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
  imp@(FImp _ cons)
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
  f_and@(FAnd left right)
  PAndI
    { proofLeft = proofL
    , proofRight = proofR
    }
  r =
    let
      (proofLeft', _) = translateP proofL left r
      (proofRight', _) = translateP proofR right r
      and' = translateF f_and r
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
      f_and = FAnd left right
      (proofAnd', _) = translateP proofAnd f_and r
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
      f_and = FAnd left right
      (proofAnd', _) = translateP proofAnd f_and r
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
  f_or@(FOr _ (FNot _))
  PLEM
  r =
    -- ~R (~R f~~ & ~R~R f~~)
    case translateF f_or r of
      or'@(FImp f_and@(FAnd left right) _) ->
        let
          hAnd = hypForm f_and
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
  f_or@(FOr left _)
  POrI1
    { proofLeft = proofLeft
    }
  r =
    -- ~r (~r left~~ & ~r right~~)
    case translateF f_or r of
      or'@(FImp f_and@(FAnd (FImp left' _) notR_right') _) ->
        let
          (proofLeft', _) = translateP proofLeft left r
          hAnd = hypForm f_and
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
translateOrI1 f p _ = error $ printf "translateOrI1: unexpected form '%s' with proof '%s'" (show f) (proofName p)

translateOrI2 :: Form -> Proof -> Form -> (Proof, Form)
translateOrI2
  f_or@(FOr _ right)
  POrI2
    { proofRight = proofRight
    }
  r =
    -- ~r (~r left~~ & ~r right~~)
    case translateF f_or r of
      or'@(FImp f_and@(FAnd notR_left' (FImp right' _)) _) ->
        let
          (proofRight', _) = translateP proofRight right r
          hAnd = hypForm f_and
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
      -- Subst porque proofForm no es dem de f, sino que asume que se hizo
      -- la subst de x por x' (esto mismo se hace en NDChecker)
      (proofForm', _) = translateP proofForm (subst x (TVar x') f) r
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
    (proofOr', FImp (FAnd (FImp left' _) (FImp right' _)) _) ->
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
    -- ~r forall x ~r g'
    formExists'@(FImp _forall@(FForall _ (FImp g' _)) _) ->
      let
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
      (proofExists', FImp (FForall _ (FImp g' _)) _) ->
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
rElim :: Form -> Proof -> R -> Proof
rElim f proof_r r = case (f, translateF f r) of
  (FFalse, _) -> proof_r
  (FTrue, FTrue) -> PTrueI
  (FPred{}, FImp (FImp _ _) _) ->
    PImpI
      { hypAntecedent = hypForm $ FImp f r
      , proofConsequent = proof_r
      }
  (FNot{}, FImp g' _) ->
    PImpI
      { hypAntecedent = hypForm g'
      , proofConsequent = proof_r
      }
  (FExists{}, FImp _forall _) ->
    PImpI
      { hypAntecedent = hypForm _forall
      , proofConsequent = proof_r
      }
  (FOr{}, FImp or' _) ->
    PImpI
      { hypAntecedent = hypForm or'
      , proofConsequent = proof_r
      }
  (FAnd left right, FAnd _ _) ->
    PAndI
      { proofLeft = rElim left proof_r r
      , proofRight = rElim right proof_r r
      }
  (FImp _ cons, FImp ant' _) ->
    PImpI
      { hypAntecedent = hypForm ant'
      , proofConsequent = rElim cons proof_r r
      }
  (FForall x g, FForall _ _) ->
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

Intuición más fácil: razonamos por el absurdo en cada sub-fórmula usando la HI.
-}
dNegRElim :: Form -> HypId -> Form -> Proof
dNegRElim f h_dneg_f' r = case (f, translateF f r) of
  (FFalse, _) ->
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
  (FPred{}, FImp (FImp _ _) _) -> tNegRElim (FImp f r) h_dneg_f' r
  (FNot{}, FImp g' _) -> tNegRElim g' h_dneg_f' r
  (FExists x _, FImp (FForall _ (FImp g' _)) _) -> tNegRElim (FForall x (FImp g' r)) h_dneg_f' r
  (FOr{}, FImp or' _) -> tNegRElim or' h_dneg_f' r
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
  (FImp _ cons, f'@(FImp ant' cons')) ->
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

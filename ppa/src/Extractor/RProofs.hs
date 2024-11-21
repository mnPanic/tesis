module Extractor.RProofs (
    rIntro,
    proofDNegRCong,
    proofNotRDistOverAndRL,
    transIntro,
    rElim,
    dNegRElim,
    tNegRElim,
) where

import Extractor.Translator.Form (translateF)
import Extractor.Types (R)
import GHC.Stack (HasCallStack)
import ND.ND (Form (..), HypId, Proof (..), Term (TVar))
import PPA.Proofs (cut, hypAndForm, hypForm)
import Text.Printf (printf)

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

-- Demuestra ~r A |- ~r A~~ por inducción estructural en A
--
-- Como no vale siempre A |- A~~ (ver Peter Selinger), y para los casos de not e
-- imp en transIntro se usa este lema, no puede valer siempre tampoco.
rIntro :: (HasCallStack) => Form -> HypId -> R -> Proof
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
transIntro :: (HasCallStack) => Form -> HypId -> R -> Proof
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

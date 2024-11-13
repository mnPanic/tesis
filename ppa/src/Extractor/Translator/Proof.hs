-- Implementa la traducción de Friedman
module Extractor.Translator.Proof (
    translateP,
    translateFriedman,
    translateF,
    translateE,
    translateContext,
) where

import Extractor.Translator.Form (translateF)
import Extractor.Types (FormPi02, R, fromPi02, splitForalls)
import ND.ND (Env (EEmpty, EExtend), Form (..), Proof (..), Term (TVar), fvP, proofName)
import ND.Subst (freshWRT, subst)
import PPA.PPA (Context, Hypothesis (..))
import PPA.Proofs (cut, hypAndForm, hypForm)
import Text.Printf (printf)

import Extractor.RProofs (dNegRElim, rElim, rIntro)

import Data.Set qualified as Set

-- Usado solo para tests
translateContext :: Context -> Form -> Context
translateContext ctx r = map (\h -> translateHyp h r) ctx

translateHyp :: Hypothesis -> Form -> Hypothesis
translateHyp hyp r = case hyp of
    HAxiom h f -> HAxiom h (translateF f r)
    HTheorem h f p -> let (p', f') = translateP p f r in HTheorem h f' p'

-- Necesario para algunos tests que hacen check con un env que no es empty
translateE :: Env -> R -> Env
translateE env r = case env of
    EEmpty -> EEmpty
    EExtend h f env2 -> EExtend h (translateF f r) (translateE env2 r)

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

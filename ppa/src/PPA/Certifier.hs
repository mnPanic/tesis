module PPA.Certifier (
    dnf,
    solveContradiction,
    toClause,
    fromClause,
    certifyBy,
    fromDNF,
    certify,
    checkContext,
    findContradiction,
    reduceContext,
    partitionForalls,
) where

import PPA.PPA (
    Case,
    Context,
    Decl (..),
    Hypothesis (HAxiom, HTheorem),
    Justification,
    Program,
    ProofStep (..),
    TProof,
    findHyp,
    fvC,
    getForm,
    getProof,
    psName,
 )

import PPA.Proofs (
    EnvItem,
    Result,
    cut,
    doubleNegElim,
    hypAndForm,
    hypForm,
    proofAndAssoc,
    proofAndCongruence1,
    proofAndCongruence2,
    proofAndDistOverOrL,
    proofAndDistOverOrR,
    proofAndEProjection,
    proofAndIList,
    proofDNegElim,
    proofImpElim,
    proofNotCongruence,
    proofNotDistOverAnd,
    proofNotDistOverOr,
    proofNotFalse,
    proofNotTrue,
    proofOrAssoc,
    proofOrCongruence1,
    proofOrCongruence2,
    wrapR,
 )

import ND.ND (
    Env (..),
    Form (..),
    HypId,
    Metavar,
    Proof (..),
    Term (TMetavar, TVar),
    VarId,
    dneg,
    fv,
    isForall,
 )

import PPA.Unifier (Substitution, showSubstitution, unifyF)

import ND.Checker (CheckResult (CheckOK), check, checkResultIsErr)
import ND.Subst (subst)

import Data.List (find, intercalate, nub, (\\))
import Data.Map qualified as Map
import Data.Set (Set)
import Text.Printf (printf)

import Data.Either (fromLeft, isLeft, isRight, lefts, rights)
import Extractor.Reducer (reduce)

reduceContext :: Context -> Context
reduceContext = map reduceHyp

reduceHyp :: Hypothesis -> Hypothesis
reduceHyp (HAxiom h f) = HAxiom h f
reduceHyp (HTheorem h f p) = HTheorem h f (reduce p)

-- En un contexto cada demostración de teorema es válida en el contexto que
-- contiene el prefijo estricto anterior a él.
checkContext :: Context -> Result ()
checkContext hs = checkContext' hs EEmpty

checkContext' :: Context -> Env -> Result ()
checkContext' [] _ = return ()
checkContext' ((HAxiom h f) : hs) e = checkContext' hs e'
  where
    e' = EExtend h f e
-- TODO: que hacer con p? esta bien agregar al contexto los teoremas como si fueran axiomas? O deberíamos cada vez que se usa insertar la demostración?
checkContext' ((HTheorem h f p) : hs) e = case check e p f of
    err | checkResultIsErr err -> Left $ printf "can't prove theorem '%s': \n%s" h (show err)
    CheckOK -> checkContext' hs e'
  where
    e' = EExtend h f e

certify :: Program -> Result Context
certify = certify' []

certify' :: Context -> Program -> Result Context
certify' ctx [] = return ctx
certify' ctx (d : ds) = do
    h <- certifyDecl ctx d
    let ctx' = ctx ++ [h]
    certify' ctx' ds

certifyDecl :: Context -> Decl -> Result Hypothesis
certifyDecl ctx d@(DAxiom{}) = certifyAxiom ctx d
certifyDecl ctx d@(DTheorem{}) = certifyTheorem ctx d

certifyAxiom :: Context -> Decl -> Result Hypothesis
-- certifyAxiom ctx a | trace (printf "certifyAxiom %s %s" (show ctx) (show a)) False = undefined
certifyAxiom ctx (DAxiom h f)
    | isRight (findHyp ctx h) = Left $ printf "axiom '%s': another axiom with the same hyp id previously declared" h
    | not (null (fv f)) = Left $ printf "axiom '%s': can't have free vars but have %s" h (showSet (fv f))
    | otherwise = return (HAxiom h f)

certifyTheorem :: Context -> Decl -> Result Hypothesis
certifyTheorem ctx (DTheorem h f p) = do
    ndProof <-
        wrapR
            (printf "theorem '%s'" h)
            (certifyProof ctx f p)
    return (HTheorem h f (PNamed h ndProof))

certifyProof :: Context -> Form -> TProof -> Result Proof
-- certifyProof ctx f ps | trace (printf "certifyProof %s %s %s" (show ctx) (show f) (show ps)) False = undefined
certifyProof _ f [] = Left $ printf "incomplete proof, still have %s as thesis" (show f)
certifyProof ctx f (p : ps) = certifyProofStep' ctx f p ps

certifyProofStep' :: Context -> Form -> ProofStep -> TProof -> Result Proof
certifyProofStep' ctx f s ps =
    either
        (Left . printf "\n certify %s: %s" (psName s))
        (Right . PNamed (show s))
        (certifyProofStep ctx f s ps)

certifyProofStep ::
    Context -> Form -> ProofStep -> TProof -> Result Proof
certifyProofStep ctx f s@(PSSuppose _ _) ps = certifySuppose ctx f s ps
certifyProofStep ctx thesis (PSHaveBy h f js) ps = do
    proof <- certifyBy ctx f js
    let ctx' = HTheorem h f proof : ctx
    certifyProof ctx' thesis ps
certifyProofStep ctx thesis (PSThusBy form js) ps = certifyThesisBy ctx thesis form js ps
certifyProofStep ctx thesis (PSEquiv thesis') ps = do
    proofThesis'ThenThesis <- solve (FImp thesis' thesis)
    proofThesis' <- certifyProof ctx thesis' ps
    return
        PImpE
            { antecedent = thesis'
            , proofImp = proofThesis'ThenThesis
            , proofAntecedent = proofThesis'
            }
certifyProofStep ctx thesis (PSClaim h f ps') ps = do
    proofClaim <- certifyProof ctx f ps'
    let ctx' = HTheorem h f proofClaim : ctx
    certifyProof ctx' thesis ps
certifyProofStep ctx thesis (PSCases js cs) ps = certifyCases ctx thesis js cs ps
certifyProofStep ctx thesis s@(PSTake{}) ps = certifyTake ctx thesis s ps
certifyProofStep ctx thesis s@(PSConsider{}) ps = certifyConsider ctx thesis s ps
certifyProofStep ctx thesis s@(PSLet{}) ps = certifyLet ctx thesis s ps

-- let Y
-- para demostrar forall X . f(X)
-- Y no debe aparecer libre en el contexto que lo precede
certifyLet :: Context -> Form -> ProofStep -> TProof -> Result Proof
certifyLet ctx forAll@(FForall x f) (PSLet y) ps
    | y `elem` fvC ctx = Left $ printf "new var (%s) must not appear free in preceding context (%s)" y (show ctx)
    | y `elem` fv forAll = Left $ printf "new var (%s) must not appear free in forall: %s" y (show forAll)
    | otherwise = do
        nextProof <- certifyProof ctx (subst x (TVar y) f) ps
        return
            PForallI
                { newVar = y
                , proofForm = nextProof
                }
certifyLet _ thesis (PSLet{}) _ =
    Left $
        printf
            "can't use with form '%s', must be an universal quantifier (forall)"
            (show thesis)

-- consider X := Y st h : f by ...
-- by debe justificar el exists X . f
-- Y no debe aparecer libre en la tesis ni en el contexto
certifyConsider :: Context -> Form -> ProofStep -> TProof -> Result Proof
-- certifyConsider ctx thesis s@(PSConsider x h f js) ps | trace (printf "certifyConsider %s %s %s %s %s %s" (show ctx) (show thesis) (show s) h (show f) (show js)) False = undefined
certifyConsider ctx thesis (PSConsider x h f js) ps
    | x `elem` fv thesis = Left $ printf "can't use an exist whose variable (%s) appears free in the thesis (%s)" x (show thesis)
    | x `elem` fvC ctx = Left $ printf "can't use an exist whose variable (%s) appears free in the preceding context (%s)" x (show ctx)
    | otherwise = do
        proofExists <- certifyBy ctx (FExists x f) js

        let ctx' = HAxiom h f : ctx
        nextProof <- certifyProof ctx' thesis ps

        return
            PExistsE
                { var = x
                , form = f
                , proofExists = proofExists
                , hyp = h
                , proofAssuming = nextProof
                }

certifyTake :: Context -> Form -> ProofStep -> TProof -> Result Proof
certifyTake ctx (FExists x f) (PSTake x' t) ps
    | x /= x' = Left $ printf "can't take var '%s', different from thesis var '%s'" x' x
    | otherwise = do
        let f' = subst x t f
        proof <- certifyProof ctx f' ps
        return
            PExistsI
                { inst = t
                , proofFormWithInst = proof
                }
certifyTake _ f (PSTake _ _) _ = Left $ printf "can't use on form '%s', not exists" (show f)

-- Certifica el suppose
certifySuppose :: Context -> Form -> ProofStep -> TProof -> Result Proof
certifySuppose ctx (FImp f1 f2) (PSSuppose name form) ps
    | form /= f1 =
        Left $
            printf
                "can't suppose '%s : %s' as it's different from antecedent '%s'"
                name
                (show form)
                (show f1)
    | otherwise = do
        let ctx' = HAxiom name form : ctx
        proofConsequent <- certifyProof ctx' f2 ps
        return
            PImpI
                { hypAntecedent = name
                , proofConsequent = proofConsequent
                }
certifySuppose ctx (FNot f) (PSSuppose name form) ps
    | form /= f =
        Left $
            printf
                "to prove by contradiction you must suppose the form without negation, but '%s' != '%s : %s'"
                (show f)
                name
                (show form)
    | otherwise = do
        let ctx' = HAxiom name form : ctx
        proofBot <- certifyProof ctx' FFalse ps
        return
            PNotI
                { hyp = name
                , proofBot = proofBot
                }
certifySuppose _ f (PSSuppose name form) _ =
    Left $
        printf
            "can't suppose '%s : %s' with form '%s', must be implication or negation"
            name
            (show form)
            (show f)

-- Certifica el cases
-- Para cada caso, sigue con la demostración asumiendo la fórmula.
certifyCases :: Context -> Form -> Justification -> [Case] -> TProof -> Result Proof
-- certifyCases ctx thesis js cases ps | trace (printf "certifyCases %s %s %s %s %s" (show ctx) (show thesis) (show js) (show cases) (show ps)) False = undefined
certifyCases _ _ _ [] _ = Left "empty cases"
certifyCases _ _ _ [_] _ = Left "must have more than one case"
certifyCases ctx thesis js cases ps = do
    let fOr = orFromCases cases
    proofOr <- certifyBy ctx fOr js
    (_, proofCases) <- certifyCases' ctx thesis proofOr cases ps
    return PNamed{name = "proof cases", proof = proofCases}

certifyCases' :: Context -> Form -> Proof -> [Case] -> TProof -> Result (HypId, Proof)
-- certifyCases' ctx thesis proofOr cases ps | trace (printf "certifyCases' %s %s %s %s %s" (show ctx) (show thesis) (show proofOr) (show cases) (show ps)) False = undefined
certifyCases' ctx thesis _ [(h, f, p)] _ = do
    proof <- certifyProof (HAxiom h f : ctx) thesis p
    return (h, proof)
certifyCases' ctx thesis proofOr cases@((h, f, p) : cs) ps = do
    let hOr = hypForm $ orFromCases cases
    let (fOrR, hOrR) = hypAndForm $ orFromCases cs

    proofAssumingF <- certifyProof (HAxiom h f : ctx) thesis p
    (hOrRest, proofRest) <- certifyCases' ctx thesis (PAx hOrR) cs ps

    return
        ( hOr
        , POrE
            { left = f
            , right = fOrR
            , proofOr = proofOr
            , hypLeft = h
            , proofAssumingLeft = proofAssumingF
            , hypRight = hOrRest
            , proofAssumingRight = proofRest
            }
        )

-- Dados los casos de un cases, devuelve el or que representan (right assoc)
orFromCases :: [Case] -> Form
orFromCases cs = fromOrListR $ map (\(_, f, _) -> f) cs

{- Certifica que f sea una parte de la tesis y una consecuencia de las justificaciones
-}
certifyThesisBy :: Context -> Form -> Form -> Justification -> TProof -> Result Proof
certifyThesisBy ctx thesis f js ps = do
    remainder <-
        wrapR "not part of thesis" $
            checkFormIncluded f thesis
    case remainder of
        -- No queda nada para demostrar, la tesis es f (modulo permutaciones)
        -- certificamos la tesis directo para evitar demostrar las permutaciones
        -- o asociatividades
        Nothing -> certifyBy ctx thesis js
        -- Tenemos que demostrar que thesis <=> f ^ f'
        Just remForms -> do
            let thesis' = FAnd f remForms
            proofThesis'ThenThesis <- solve (FImp thesis' thesis)

            proofF <- certifyBy ctx f js
            -- Por acá continua
            proofRemForms <- certifyProof ctx remForms ps

            return
                PImpE
                    { antecedent = thesis'
                    , proofImp = proofThesis'ThenThesis
                    , proofAntecedent =
                        PAndI
                            { proofLeft = proofF
                            , proofRight = proofRemForms
                            }
                    }

-- Chequea que f este incluido en g y devuelve la diferencia
-- Por ejemplo
--  checkFormIncluded (a ^ b) ^ ((c ^ d) ^ e) (a ^ e) -> Right b ^ (c ^ d)
checkFormIncluded :: Form -> Form -> Result (Maybe Form)
checkFormIncluded f g
    | subset fL gL = return $ case nub gL \\ fL of
        [] -> Nothing
        fs -> Just $ fromAndList fs
    | otherwise = Left $ printf "%s (%s) not contained in %s (%s)" (show f) (show fL) (show g) (show gL)
  where
    (fL, gL) = (toAndList f, toAndList g)

subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

toAndList :: Form -> [Form]
toAndList (FAnd f1 f2) = toAndList f1 ++ toAndList f2
toAndList f = [f]

-- Left assoc
fromAndList :: [Form] -> Form
fromAndList = foldl1 FAnd

-- Right assoc
fromOrListR :: [Form] -> Form
fromOrListR = foldr1 FOr

{- Certifica que js |- f

Si las justificaciones son [h1, .., hn] y el contexto tiene {h1: f1, ... hn:
fn}, da una demostración de que

    f1, ..., fn |- f

pero en realidad por abajo demuestra

    (f1 ^ ... ^ fn) => f
-}
certifyBy :: Context -> Form -> Justification -> Result Proof
-- certifyBy ctx f js | trace (printf "certifyBy %s %s %s" (show ctx) (show f) (show js)) False = undefined
certifyBy _ f [] = solve f
certifyBy ctx f js = do
    jsHyps <- findJustification ctx js
    let jsForms = map getForm jsHyps

    let antecedent = fromClause jsForms

    proofAntecedentImpF <- solve (FImp antecedent f)

    return
        -- Dem de f con (f1 ... fn) => f
        PImpE
            { antecedent = antecedent
            , -- Dem de (f1 ... fn) => f por el absurdo
              proofImp = PNamed "by: proof justification implies form" proofAntecedentImpF
            , -- Dem de f1 ^ ... ^ fn
              proofAntecedent = PNamed "by: proof justification" $ proofAndIList (map getProof jsHyps)
            }

findJustification :: Context -> Justification -> Result [Hypothesis]
findJustification ctx js
    | not (null missingHyps) =
        Left $ "finding hyps in context: " ++ intercalate "; " (lefts $ map snd missingHyps)
    | otherwise = Right (rights $ map snd hyps)
  where
    hyps = zip js $ map (findHyp ctx) js
    missingHyps = filter (\(_, r) -> isLeft r) hyps

{- solve encuentra una demostración automáticamente para thesis.

Lo hace por el absurdo, la niega, la pasa a DNF y encuentra una
contradicción. Este procedimiento es completo para proposicional y heurístico
para LPO.
-}
solve :: Form -> Result Proof
-- solve thesis | trace (printf "solve %s" (show thesis)) False = undefined
solve thesis = do
    let fNotThesis = FNot thesis
    let hNotThesis = hypForm fNotThesis

    let (fDNFNotThesis, dnfProof) = dnf (hNotThesis, fNotThesis)

    let hDNFNotThesis = hypForm fDNFNotThesis

    contradictionProof <-
        wrapR (printf "solving form by finding contradiction of negation:\n'%s',\nin dnf: '%s'" (show fNotThesis) (show fDNFNotThesis)) $
            solveContradiction (hDNFNotThesis, fDNFNotThesis)

    -- Dem de thesis por el absurdo
    return
        PImpE
            { antecedent = dneg thesis
            , proofImp = doubleNegElim thesis
            , proofAntecedent =
                PNotI
                    { hyp = hNotThesis
                    , -- Demostración de bottom (contradicción) asumiendo que no vale
                      -- la tesis. Primero convertimos a DNF y luego demostramos que
                      -- la version en DNF es refutable.
                      proofBot =
                        PNamed "solver: cut proof dnf contradiction" $
                            cut
                                fDNFNotThesis
                                dnfProof
                                hDNFNotThesis
                                (PNamed "solver: contradiction" contradictionProof)
                    }
            }

{- dnf

Dada una fórmula F y una versión en DNF F' (no es única), da una demostración de
F |- F'.
-}
dnf :: EnvItem -> (Form, Proof)
dnf i = (f', PNamed "dnf" p)
  where
    (f', p) = convertToDnf i

convertToDnf :: EnvItem -> (Form, Proof)
convertToDnf i@(hF, f) = case dnfStep i of
    -- f está en DNF, caso base
    Nothing -> (f, PAx hF)
    -- f |- f' en un paso
    Just ((hF', f'), pFThenF', _) ->
        (f'', cut f' pFThenF' hF' pF'ThenF'')
      where
        (f'', pF'ThenF'') = convertToDnf (hF', f')

{- dnfStep hace una transformación "small-step" de una fórmula hacia DNF.
Dada una fórmula F, devuelve la fórmula F' con un paso aplicado, y las
demostraciones de F |- F' y F' |- F (necesarias para la congruencia, dado que
algunos operadores como el Not son opuestos).

Devuelve Nothing cuando F ya está en DNF.
-}
-- TODO: Raro que tomo la hip de l pero devuelvo la hip de l', porque se genera
-- acá y no se conoce de antemano. Se podrían devolver siempre acá las hips?
-- Pero es menos general.
dnfStep :: EnvItem -> Maybe (EnvItem, Proof, Proof)
-- dnfStep i | trace (printf "dnfStep %s" (show i)) False = undefined
{- Casos de reescritura -}
-- x ^ (y v z) -|- (x ^ y) v (x ^ z)
dnfStep (hAnd, FAnd x (FOr y z)) =
    Just
        ( (hOr, fOr)
        , pAndToOrLLR
        , pAndToOrLRL
        )
  where
    fOr = FOr (FAnd x y) (FAnd x z)
    hOr = hypForm fOr
    (pAndToOrLLR, pAndToOrLRL) = proofAndDistOverOrL x y z hAnd hOr
-- (y v z) ^ x -|- (y ^ x) v (z ^ x)
dnfStep (hAnd, FAnd (FOr y z) x) =
    Just
        ( (hOr, fOr)
        , pAndToOrRLR
        , pAndToOrRRL
        )
  where
    fOr = FOr (FAnd y x) (FAnd z x)
    hOr = hypForm fOr
    (pAndToOrRLR, pAndToOrRRL) = proofAndDistOverOrR x y z hAnd hOr
-- ~T -|- F
dnfStep (hNotTrue, FNot FTrue) =
    Just
        ( (hFalse, FFalse)
        , pNotTrueThenFalse
        , pFalseThenNotTrue
        )
  where
    hFalse = hypForm FFalse
    (pNotTrueThenFalse, pFalseThenNotTrue) = proofNotTrue hNotTrue hFalse
-- ~F -|- T
dnfStep (hNotFalse, FNot FFalse) =
    Just
        ( (hTrue, FTrue)
        , pNotFalseThenTrue
        , pTrueThenNotFalse
        )
  where
    hTrue = hypForm FTrue
    (pNotFalseThenTrue, pTrueThenNotFalse) = proofNotFalse hNotFalse hTrue

-- x v (y v z) -|- (x v y) v z
dnfStep (hOrR, FOr x (FOr y z)) =
    Just
        ( (hOrL, fOrL)
        , pOrAssocRL
        , pOrAssocLR
        )
  where
    fOrL = FOr (FOr x y) z
    hOrL = hypForm fOrL
    (pOrAssocLR, pOrAssocRL) = proofOrAssoc x y z hOrL hOrR
-- x ^ (y ^ z) -|- (x ^ y) ^ z
dnfStep (hAndR, FAnd x (FAnd y z)) =
    Just
        ( (hAndL, fAndL)
        , pAndAssocRL
        , pAndAssocLR
        )
  where
    fAndL = FAnd (FAnd x y) z
    hAndL = hypForm fAndL
    (pAndAssocLR, pAndAssocRL) = proofAndAssoc x y z hAndL hAndR
-- ~~x -|- x
dnfStep (_, FNot (FNot x)) =
    Just
        ( (hX, x)
        , pDNegElimLR
        , pDNegElimRL
        )
  where
    hX = hypForm x
    hDNegX = hypForm $ dneg x
    (pDNegElimLR, pDNegElimRL) = proofDNegElim x hX hDNegX
-- x => y -|- ~x v y
dnfStep (hImp, FImp x y) =
    Just
        ( (hOr, fOr)
        , pImpElimLR
        , pImpElimRL
        )
  where
    fOr = FOr (FNot x) y
    hOr = hypForm fOr
    (pImpElimLR, pImpElimRL) = proofImpElim x y hImp hOr
-- ~(x ^ y) -|- ~x v ~y
dnfStep (hNot, FNot (FAnd x y)) =
    Just
        ( (hOr, fOr)
        , pNotDistOverAndLR
        , pNotDistOverAndRL
        )
  where
    fOr = FOr (FNot x) (FNot y)
    hOr = hypForm fOr
    (pNotDistOverAndLR, pNotDistOverAndRL) = proofNotDistOverAnd x y hNot hOr
-- ~(x v y) -|- ~x ^ ~y
dnfStep (hNotOr, FNot (FOr x y)) =
    Just
        ( (hAnd, fAnd)
        , pNotDistOverOrLR
        , pNotDistOverOrRL
        )
  where
    fAnd = FAnd (FNot x) (FNot y)
    hAnd = hypForm fAnd
    (pNotDistOverOrLR, pNotDistOverOrRL) = proofNotDistOverOr x y hNotOr hAnd
{- Casos de congruencia -}
-- Un AND puede ser una cláusula válida (con otros ands de literales) o
-- tener que ser transformada recursivamente.
dnfStep (hAnd, FAnd l r) = case dnfStep (hL, l) of
    -- l |- l'
    Just ((hL', l'), pLThenL', pL'ThenL) ->
        Just
            ( (hAnd', fAnd')
            , pAndCong1LR
            , pAndCong1RL
            )
      where
        fAnd' = FAnd l' r
        hAnd' = hypForm fAnd'
        (pAndCong1LR, pAndCong1RL) = proofAndCongruence1 l r l' hAnd hAnd' hL pLThenL' hL' pL'ThenL
    Nothing -> case dnfStep (hR, r) of
        -- Cláusula válida, está en DNF
        Nothing -> Nothing
        -- r |- r'
        Just ((hR', r'), pRThenR', pR'ThenR) ->
            Just
                ( (hAnd', fAnd')
                , pAndCong2LR
                , pAndCong2RL
                )
          where
            fAnd' = FAnd l r'
            hAnd' = hypForm fAnd'
            (pAndCong2LR, pAndCong2RL) = proofAndCongruence2 l r r' hAnd hAnd' hR pRThenR' hR' pR'ThenR
  where
    hL = hypForm l
    hR = hypForm r
-- Un OR puede estar en DNF (compuesto por cláusulas válidas) o tener que ser
-- transformado recursivamente
dnfStep (hOr, FOr l r) = case dnfStep (hL, l) of
    -- l |- l'
    Just ((hL', l'), pLThenL', pL'ThenL) ->
        Just
            ( (hOr', fOr')
            , pOrCong1LR
            , pOrCong1RL
            )
      where
        fOr' = FOr l' r
        hOr' = hypForm fOr'
        (pOrCong1LR, pOrCong1RL) = proofOrCongruence1 l r l' hOr hOr' hL pLThenL' hL' pL'ThenL
    Nothing -> case dnfStep (hR, r) of
        -- Fórmula válida, está en DNF
        Nothing -> Nothing
        -- r |- r'
        Just ((hR', r'), pRThenR', pR'ThenR) ->
            Just
                ( (hOr', fOr')
                , pOrCong2LR
                , pOrCong2RL
                )
          where
            fOr' = FOr l r'
            hOr' = hypForm fOr'
            (pOrCong2LR, pOrCong2RL) = proofOrCongruence2 l r r' hOr hOr' hR pRThenR' hR' pR'ThenR
  where
    hL = hypForm l
    hR = hypForm r
-- Not
dnfStep (hNot, FNot f) = case dnfStep (hF, f) of
    Nothing -> Nothing
    Just ((hF', f'), pFThenF', pF'ThenF) ->
        Just
            ( (hNot', fNot')
            , pNotCongLR
            , pNotCongRL
            )
      where
        fNot' = FNot f'
        hNot' = hypForm fNot'
        (pNotCongLR, pNotCongRL) = proofNotCongruence f f' hNot hNot' hF pFThenF' hF' pF'ThenF
  where
    hF = hypForm f
-- Resto, literales o errores
dnfStep (h, f)
    | isLiteral f = Nothing
    | otherwise = error $ printf "unexpected case '%s':'%s'" h (show f)

{- solveContradiction demuestra una contradicción de una fórmula que se asume
que está en DNF. Para ello refuta cada cláusula, buscando o el mismo literal
negado y sin negar, que tenga false, o instanciando cuantificadores universales
y re-convirtiendo a dnf.

Devuelve una demostración de
    f |- bot
-}
solveContradiction :: EnvItem -> Result Proof
solveContradiction (hOr, FOr l r) = do
    let hLeft = hOr ++ " L"
    let hRight = hOr ++ " R"
    proofLeft <- solveContradiction (hLeft, l)
    proofRight <- solveContradiction (hRight, r)
    return
        POrE
            { left = l
            , right = r
            , proofOr = PAx hOr
            , hypLeft = hLeft
            , proofAssumingLeft = proofLeft
            , hypRight = hRight
            , proofAssumingRight = proofRight
            }
solveContradiction i = solveClause i

{- solveClause intenta demostrar que una cláusula es contradictoria.

    p1 ^ p2 ^ ... ^ p_n |- false

Asume que la fórmula es una cláusula: Una conjunción de literales (predicados,
true/false y cuantificadores)

Hay 3 casos

- Algún literal es false
- Hay dos literales opuestos
- Sino, intenta eliminando universales
-}
solveClause :: EnvItem -> Result Proof
solveClause (h, rawClause) = do
    clause <- toClause rawClause
    case findContradiction clause of
        Just FFalse -> do
            proofFalse <- proofAndEProjection (h, rawClause) FFalse
            return
                ( PNamed
                    (printf "contradiction of %s by false" (show rawClause))
                    proofFalse
                )
        Just f -> do
            proofF <- proofAndEProjection (h, rawClause) f
            proofNotF <- proofAndEProjection (h, rawClause) (FNot f)
            return
                ( PNamed
                    (printf "contradiction of %s by %s and %s" (show rawClause) (show f) (show $ FNot f))
                    ( PNotE
                        { form = f
                        , proofNotForm = proofNotF
                        , proofForm = proofF
                        }
                    )
                )
        -- No contradicting literals or false, try by eliminating foralls
        Nothing ->
            wrapR
                (printf "'%s' contains no contradicting literals or false, and trying to eliminate foralls" (show rawClause))
                (trySolveClauseElimForall (h, rawClause))

-- Clause es una conjunción de literales
type Clause = [Form]

-- Encuentra dos literales opuestos o false. Devuelve una sola fórmula, que es o
-- bien false o una que se contradice con su negación.
findContradiction :: Clause -> Maybe Form
findContradiction fs
    -- Contradicción por false
    | FFalse `elem` fs = Just FFalse
    -- No hay por false, buscamos dos opuestas
    | otherwise = case filter hasOpposite fs of
        (f : _) -> Just f
        [] -> Nothing
  where
    hasOpposite f = FNot f `elem` fs

{- trySolveClauseElimForall intenta de refutar una cláusula eliminando a lo sumo
un forall.

    p1 ^ p2 ^ ... ^ p_n |- false

Para ello, para cada forall intenta:

- instancia la variable en una metavariable, que es única porque solo
  hacemos este proceso una vez (pega la demo con PForallE reemplazando con lo
  que unifique)

- como el pasaje a dnf no se mete en los cuantificadores, la fórmula resultante
  puede no estar en dnf. re-convierte a dnf.

- resuelve la cláusula pero teniendo en cuenta que en lugar de igualdad de
  términos, hay que ver que unifiquen (y propagar la sustitución para arriba)
-}
trySolveClauseElimForall :: EnvItem -> Result Proof
trySolveClauseElimForall (h, rawClause) =
    do
        cl <- toClause rawClause
        let foralls = partitionForalls cl
        if null foralls
            then Left "no foralls to eliminate"
            else do
                let allProofs = map (solveClauseElimForall h) foralls
                case find isRight allProofs of
                    Nothing ->
                        let
                            errs = map (fromLeft "") allProofs
                            errMsgs = intercalate "\n" errs
                         in
                            Left ("no foralls useful for contradictions:\n" ++ errMsgs)
                    Just (Right proof) -> Right proof

partitionForalls :: Clause -> [(Clause, Form, Clause)]
partitionForalls cl = nub $ filter (\(_, f, _) -> isForall f) (allPartitions cl)

allPartitions :: Clause -> [(Clause, Form, Clause)]
allPartitions [f] = [([], f, [])]
allPartitions (f : fs) =
    let
        rest = allPartitions fs
        appendLeft = map (\(cL, g, cR) -> (f : cL, g, cR)) rest
        asCenter = map (\(cL, g, cR) -> ([], f, cL ++ g : cR)) rest
     in
        appendLeft ++ asCenter

{- solveClauseElimForall refuta una cláusula mediante la eliminación de los N
foralls de una cláusula

- Encuentra la sustitución que lleva a una contradicción, sin generar
  demostraciones, en un modo "dry run"

- Realiza esa sustitución y encuentra la contradicción de forma usual, sin
  eliminar foralls.
-}
solveClauseElimForall :: HypId -> (Clause, Form, Clause) -> Result Proof
solveClauseElimForall h (cL, f, cR) =
    wrapR
        (printf "try eliminating '%s'" (show f))
        (solveClauseElimForall' h (cL, f, cR))

elimForalls :: Form -> Substitution -> Map.Map VarId Metavar -> Form
elimForalls f@(FForall x g) s m = case Map.lookup x m of
    -- No fue necesario eliminar más foralls
    Nothing -> f
    -- Se eliminó el forall
    Just meta ->
        -- Si no está en la sustitución, no fue necesario unificar la variable
        -- para la contradicción, por ej. f(a) & forall X. ~f(a)
        let x' = case Map.lookup meta s of
                Nothing -> TVar x
                Just t -> t
            g' = subst x x' g
         in elimForalls g' s m
elimForalls g _ _ = g

solveClauseElimForall' :: HypId -> (Clause, Form, Clause) -> Result Proof
-- solveClauseElimForall' hClause t | trace (printf "solveClauseElimForall' h:'%s' t:%s" hClause (show t)) False = undefined
solveClauseElimForall' hClause (cL, f@(FForall{}), cR) =
    do
        -- Primero encontramos por qué reemplazar x para que sea refutable
        (sub, usedVars) <- findSubstToSolveContradiction cL cR f 0

        -- Ya sé por qué tengo que sustituir la var del forall cuando lo
        -- elimine, así que lo pego con cut a la fórmula con eso eliminado y
        -- sigo por ahí
        let g' = elimForalls f sub usedVars
        let (clReplaced, hclReplaced) = hypAndForm $ fromClause $ cL ++ g' : cR

        let (dnfClReplaced, proofDnfClReplaced) = dnf (hclReplaced, clReplaced)
        let hdnfClReplaced = hypForm dnfClReplaced

        contradictionProof <- solveContradiction (hdnfClReplaced, dnfClReplaced)

        let origClause = cL ++ f : cR
        proofClReplacedList <- proveClauseWithForallReplaced (hClause, fromClause origClause) sub usedVars origClause f
        let proofClReplaced = proofAndIList proofClReplacedList
        -- TODO: forall repetido?

        let proofDNFToContradiction =
                PNamed "dnf to contradiction" $
                    cut
                        dnfClReplaced
                        proofDnfClReplaced
                        hdnfClReplaced
                        contradictionProof

        return
            ( PNamed
                "forall elimination to dnf"
                ( cut
                    clReplaced
                    proofClReplaced
                    hclReplaced
                    proofDNFToContradiction
                )
            )

{-
proveClauseWithForallReplaced demuestra la cláusula resultante de eliminar N
foralls contiguos

Por ejemplo,

    f1 & f2 & (forall x. forall y. forall z. f(x) & f(y) & f(z)) & f3 |- f1 & f2 & f(a) & f(b) & f(c) & f3
-}
proveClauseWithForallReplaced :: EnvItem -> Substitution -> Map.Map VarId Metavar -> Clause -> Form -> Result [Proof]
proveClauseWithForallReplaced _ _ _ [] _ = Right []
proveClauseWithForallReplaced clause sub usedVars (f : fs) fForall@(FForall{}) =
    do
        proofRest <- proveClauseWithForallReplaced clause sub usedVars fs fForall
        if f == fForall
            then do
                proofForall <- proofAndEProjection clause fForall
                let proofForallReplaced = proveForallWithElim fForall sub usedVars proofForall

                return (proofForallReplaced : proofRest)
            else do
                proofF <- proofAndEProjection clause f
                return (proofF : proofRest)

-- TODO
{- Demuestra

Dado {x = a, y = b, z = c}, demuestra

    forall x. forall y. forall z. f(x) & f(y) & f(z) |- f(a) & f(b) & f(c)
-}
proveForallWithElim :: Form -> Substitution -> Map.Map VarId Metavar -> Proof -> Proof
proveForallWithElim (FForall x g) s m proofForall = case Map.lookup x m of
    -- No fue necesario eliminar más foralls
    Nothing -> proofForall
    -- Se eliminó el forall
    Just meta -> do
        -- Si no está en la sustitución, no fue necesario unificar la variable
        -- para la contradicción, por ej. f(a) & forall X. ~f(a)
        let x' = case Map.lookup meta s of
                Nothing -> TVar x
                Just t -> t
        let (g', h_g') = hypAndForm $ subst x x' g
        let proofGThenG' =
                PForallE
                    { var = x
                    , form = g
                    , proofForall = proofForall
                    , termReplace = x'
                    }
        let proofG' = proveForallWithElim g' s m (PAx h_g')
        cut g' proofGThenG' h_g' proofG'
proveForallWithElim _ _ _ proofForm = proofForm

{-
Como el contenido de forall pegado con el resto de la cláusula puede no
resultar en una fórmula en DNF, hay que re-convertir

Por ej.
    (forall x . a | b) & c
    --> (a|b) & c

que no está en dnf

    (dnf) (a & c) | (b & c)
-}
findSubstToSolveContradiction :: Clause -> Clause -> Form -> Metavar -> Result (Substitution, Map.Map VarId Metavar)
findSubstToSolveContradiction clL clR (FForall x g) metavar = do
    -- Reemplazo la variable por una metavariable así unificando encontramos la
    -- sustitución que permite refutarla (en caso de que exista)
    let meta = TMetavar metavar
    let gMeta = subst x meta g
    let clMeta = fromClause $ clL ++ [gMeta] ++ clR

    -- Convierto la nueva cláusula a DNF
    let (dnfClMeta, _) = dnf ("h", clMeta) -- No importa la demo
    case solveContradictionUnifying Map.empty dnfClMeta of
        Right (s : _) -> Right (s, Map.singleton x metavar)
        Left err -> case findSubstToSolveContradiction clL clR gMeta (metavar + 1) of
            Left err2 ->
                Left $
                    printf
                        "solving clause with '%s' replaced by metavar '%s' and reconverting to dnf \n%s\n%s\n%s"
                        x
                        (show meta)
                        (mustShowDNF dnfClMeta)
                        err
                        err2
            Right (s, ms) -> Right (s, Map.insert x metavar ms)
findSubstToSolveContradiction _ _ _ _ = Left "no more foralls"

{- solveContradictionUnifying encuentra la sustitución que hace a la fórmula
refutable (en caso de que exista).
Asume que la fórmula está en DNF.

Puede haber más de una combinación de fórmulas posible para cada cláusula, y es
importante considerarlas todas. Por ej.

    [
        [f(?), ~f(a), ~f(b)]  -> {? = a}, {? = b}
        [f(?), ~f(b) ]        -> {? = b}
    ]

si no se consideran todas las sustituciones candidatas, en la primera se
commitea a {? = a} y la 2da fallaría.
-}
solveContradictionUnifying :: Substitution -> Form -> Result [Substitution]
solveContradictionUnifying s (FOr l r) = do
    sL <- solveContradictionUnifying s l
    solveContradictionUnifyingWithMany sL r
solveContradictionUnifying s i = solveClauseUnifying s i

solveContradictionUnifyingWithMany :: [Substitution] -> Form -> Result [Substitution]
solveContradictionUnifyingWithMany ss f =
    let substs = map (`solveContradictionUnifying` f) ss
     in case filter isRight substs of
            [] -> Left (intercalate "\n" (lefts substs))
            _ -> Right (concat (rights substs))

-- Encuentra las sustituciones que permite una contradicción: dos literales
-- opuestos que unifican o false.
solveClauseUnifying :: Substitution -> Form -> Result [Substitution]
solveClauseUnifying s rawClause = do
    clause <- toClause rawClause
    -- Contradicción por false
    if FFalse `elem` clause
        then return [s]
        else -- No hay por false, buscamos dos opuestas que unifiquen
            case concatMap (findAllUnifyingWithOpposite s clause) clause of
                [] -> Left $ printf "(subst %s) no opposites that unify in clause %s" (showSubstitution s) (show clause)
                ss -> Right ss

-- Devuelve todas las sustituciones que hacen que la fórmula f unifique con su
-- opuesto
findAllUnifyingWithOpposite :: Substitution -> [Form] -> Form -> [Substitution]
findAllUnifyingWithOpposite s (f' : fs) f =
    ( case unifyF s (FNot f) f' of
        Left _ -> []
        Right s' -> [s']
    )
        ++ findAllUnifyingWithOpposite s fs f
findAllUnifyingWithOpposite _ [] _ = []

toClause :: Form -> Result Clause
toClause (FAnd l r) = do
    clauseL <- toClause l
    clauseR <- toClause r
    return (clauseL ++ clauseR)
toClause f
    | isLiteral f = Right [f]
    | otherwise = Left $ printf "convert to clause: %s is not a literal" (show f)

-- left assoc
fromClause :: Clause -> Form
fromClause = foldl1 FAnd

-- left assoc
fromDNF :: [Clause] -> Form
fromDNF cs = foldl1 FOr (map fromClause cs)

mustShowDNF :: Form -> String
mustShowDNF f = case showDNF f of
    Right s -> s
    Left e -> error e

showDNF :: Form -> Result String
showDNF f = do
    dnfF <- toDNFMatrix f
    return (intercalate "\n" (map show dnfF))

toDNFMatrix :: Form -> Result [Clause]
toDNFMatrix (FOr l r) = do
    dnfL <- toDNFMatrix l
    dnfR <- toDNFMatrix r
    return (dnfL ++ dnfR)
toDNFMatrix f = do
    clauseF <- toClause f
    return [clauseF]

-- True si es un literal
isLiteral :: Form -> Bool
isLiteral (FNot f) = isNotLiteral f
isLiteral (FPred _ _) = True
isLiteral FTrue = True
isLiteral FFalse = True
isLiteral (FForall _ _) = True
isLiteral (FExists _ _) = True
isLiteral _ = False

-- True si ~F es un literal
isNotLiteral :: Form -> Bool
isNotLiteral (FPred _ _) = True
isNotLiteral (FForall _ _) = True
isNotLiteral (FExists _ _) = True
isNotLiteral _ = False

showSet :: Data.Set.Set String -> String
showSet s = "{" ++ intercalate "," (foldr (:) [] s) ++ "}"
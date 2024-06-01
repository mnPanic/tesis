module Certifier (
    dnf,
    solveContradiction,
    toClause,
    fromClause,
    certifyBy,
    fromDNF,
    certify,
    checkContext,
    findContradiction,
) where

import PPA (
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
    getHypId,
    getProof,
 )

import NDProofs (
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

import ND (
    Env (..),
    Form (..),
    HypId,
    Proof (..),
    Term (TFun, TMetavar, TVar),
    dneg,
    fv,
    isForall,
 )

import Unifier (SingleSubst (..), unifyF, unifyT)

import NDChecker (CheckResult (CheckOK), check, checkResultIsErr, subst)

import Data.List (find, intercalate, nub, partition, (\\))
import Data.Maybe (fromJust, isJust, isNothing)
import Text.Printf (printf)

import Data.Either (fromLeft, fromRight, isLeft, isRight, lefts)
import Debug.Trace

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
certifyAxiom ctx (DAxiom h f) = return (HAxiom h f)

certifyTheorem :: Context -> Decl -> Result Hypothesis
certifyTheorem ctx (DTheorem h f p) = do
    ndProof <- certifyProof ctx f p
    return (HTheorem h f ndProof)

certifyProof :: Context -> Form -> TProof -> Result Proof
-- certifyProof ctx f ps | trace (printf "certifyProof %s %s %s" (show ctx) (show f) (show ps)) False = undefined
certifyProof ctx f [] = Left $ printf "incomplete proof, still have %s as thesis" (show f)
certifyProof ctx f (p : ps) = certifyProofStep ctx f p ps

certifyProofStep ::
    Context -> Form -> ProofStep -> TProof -> Result Proof
certifyProofStep ctx f s@(PSSuppose _ _) ps = certifySuppose ctx f s ps
certifyProofStep ctx thesis (PSHaveBy h f js) ps = do
    proof <- certifyBy ctx f js
    let ctx' = HTheorem h f proof : ctx
    certifyProof ctx' thesis ps
certifyProofStep ctx thesis (PSThusBy form js) ps =
    certifyThesisBy ctx thesis form js ps
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

-- let X := Y
-- para demostrar forall X . f
-- X no debe aparecer libre en el contexto que lo precede
certifyLet :: Context -> Form -> ProofStep -> TProof -> Result Proof
certifyLet ctx (FForall x f) (PSLet x' y) ps
    | x /= x' = Left $ printf "let: assinged var (%s) must be the same as in thesis (%s)" x' x -- TODO: cambiar
    | y `elem` fvC ctx = Left $ printf "let: new var (%s) must not appear free in preceding context (%s)" y (show ctx)
    | otherwise = do
        nextProof <- certifyProof ctx (subst x (TVar y) f) ps
        return PForallI{proofForm = nextProof}
certifyLet ctx thesis (PSLet x y) ps =
    Left
        $ printf
            "let: can't use with form '%s', must be an universal quantifier (forall)"
            (show thesis)

-- consider X st h : f by ...
-- by debe justificar el exists X . f
-- X no debe aparecer libre en la tesis ni en el contexto
certifyConsider :: Context -> Form -> ProofStep -> TProof -> Result Proof
certifyConsider ctx thesis (PSConsider x h f js) ps
    | x `elem` fv thesis = Left $ printf "consider: can't use an exist whose variable (%s) appears free in the thesis (%s)" x (show thesis)
    | x `elem` fvC ctx = Left $ printf "consider: can't use an exist whose variable (%s) appears free in the preceding context (%s)" x (show ctx)
    | otherwise = do
        proofExists <- certifyBy ctx (FExists x f) js
        -- TODO: checks que fallarian en ND
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
    | x /= x' = Left $ printf "take: can't take var '%s', different from thesis var '%s'" x' x
    | otherwise = do
        let f' = subst x t f
        proof <- certifyProof ctx f' ps
        return
            PExistsI
                { inst = t
                , proofFormWithInst = proof
                }
certifyTake _ f (PSTake _ _) _ = Left $ printf "take: can't use on form '%s', not exists" (show f)

-- Certifica el suppose
certifySuppose :: Context -> Form -> ProofStep -> TProof -> Result Proof
certifySuppose ctx (FImp f1 f2) (PSSuppose name form) ps
    | form /= f1 =
        Left
            $ printf
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
        Left
            $ printf
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
certifySuppose ctx f (PSSuppose name form) ps =
    Left
        $ printf
            "can't use command 'suppose %s : %s' with form '%s', must be implication or negation"
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
certifyCases' ctx thesis _ [(h, f, p)] ps = do
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
orFromCases cs = fromOrListR $ map (\(h, f, p) -> f) cs

{- Certifica que f sea una parte de la tesis y una consecuencia de las justificaciones
-}
certifyThesisBy :: Context -> Form -> Form -> Justification -> TProof -> Result Proof
certifyThesisBy ctx thesis f js ps = do
    remainder <-
        wrapR "not part of thesis"
            $ checkFormIncluded f thesis
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
certifyBy ctx f [] = solve f
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
              proofImp = proofAntecedentImpF
            , -- Dem de f1 ^ ... ^ fn
              proofAntecedent = proofAndIList (map getProof jsHyps)
            }

findJustification :: Context -> Justification -> Result [Hypothesis]
findJustification ctx js
    | not (null missingHyps) =
        Left $ "finding hyps in context: " ++ intercalate "; " (map (\(h, Left err) -> err) missingHyps)
    | otherwise = Right $ map (\(h, Right hyp) -> hyp) hyps
  where
    hyps = zip js $ map (findHyp ctx) js
    missingHyps = filter (\(h, r) -> isLeft r) hyps

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
        wrapR (printf "finding contradiction for dnf form '%s' obtained from '%s'" (show fDNFNotThesis) (show fNotThesis))
            $ solveContradiction (hDNFNotThesis, fDNFNotThesis)

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
                        cut
                            fDNFNotThesis
                            dnfProof
                            hDNFNotThesis
                            contradictionProof
                    }
            }

{- dnf

Dada una fórmula F y una versión en DNF F' (no es única), da una demostración de
F |- F'.
-}
dnf :: EnvItem -> (Form, Proof)
dnf i@(h, f) = (f', PNamed "dnf" p)
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
dnfStep (hDNeg, FNot (FNot x)) =
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
        let foralls = filter isForall cl
        if null foralls
            then Left "form contains no foralls to eliminate"
            else do
                let allProofs = zip foralls (map (solveClauseElimForall (h, rawClause)) foralls)
                case find (isRight . snd) allProofs of
                    Nothing ->
                        let
                            errs = map (fromLeft "" . snd) allProofs
                            errMsgs = intercalate "\n" errs
                         in
                            Left ("no foralls useful for contradictions:\n" ++ errMsgs)
                    Just (_, Right proof) -> Right proof

{- solveClauseElimForall refuta una cláusula mediante la eliminación de un
forall.

- Genera la demostración de la refutación generando la sustitución, pero la demo
  asume que ya está sustituido

- P

-}
solveClauseElimForall :: EnvItem -> Form -> Result Proof
solveClauseElimForall i f =
    wrapR
        (printf "try eliminating '%s'" (show f))
        (solveClauseElimForall' i f)

solveClauseElimForall' :: EnvItem -> Form -> Result Proof
solveClauseElimForall' (hClause, rawClause) f@(FForall x g) =
    do
        -- Primero encontramos por qué reemplazar x para que sea refutable
        cl <- toClause rawClause
        sub <- findSubstToSolveContradiction cl f
        let x' = case sub of SSEmpty -> TVar x; SSTerm t -> t

        -- Ya sé por qué tengo que sustituir la var del forall cuando lo
        -- elimine, así que lo pego con cut a la fórmula con eso eliminado y
        -- sigo por ahí
        let gReplaced = subst x x' g
        let (clReplaced, hclReplaced) = hypAndForm $ fromClause $ replaceFirst cl f gReplaced

        let (dnfClReplaced, proofDnfClReplaced) = dnf (hclReplaced, clReplaced)
        let hdnfClReplaced = hypForm dnfClReplaced

        contradictionProof <- solveContradiction (hdnfClReplaced, dnfClReplaced)

        proofClReplacedList <- proveClauseWithForallReplaced (hClause, rawClause) x' cl f
        let proofClReplaced = proofAndIList proofClReplacedList
        -- TODO: forall repetido?

        let proofDNFToContradiction =
                PNamed "dnf to contradiction"
                    $ cut
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
proveClauseWithForallReplaced demuestra la cláusula resultante de eliminar un
forall

Por ejemplo,

    f1 & f2 & (forall x. f(x)) & f3 |- f1 & f2 & f(a) & f3
-}
proveClauseWithForallReplaced :: EnvItem -> Term -> Clause -> Form -> Result [Proof]
proveClauseWithForallReplaced _ _ [] _ = Right []
proveClauseWithForallReplaced clause newTerm (f : fs) fForall@(FForall x g) =
    do
        proofRest <- proveClauseWithForallReplaced clause newTerm fs fForall
        if f == fForall
            then do
                proofForall <- proofAndEProjection clause fForall
                let proofForallReplaced =
                        PForallE
                            { var = x
                            , form = g
                            , proofForall = proofForall
                            , termReplace = newTerm
                            }

                return (proofForallReplaced : proofRest)
            else do
                proofF <- proofAndEProjection clause f
                return (proofF : proofRest)

{-
Como el contenido de forall pegado con el resto de la cláusula puede no
resultar en una fórmula en DNF, hay que re-convertir

Por ej.
    (forall x . a | b) & c
    --> (a|b) & c

que no está en dnf

    (dnf) (a & c) | (b & c)
-}
findSubstToSolveContradiction :: Clause -> Form -> Result SingleSubst
findSubstToSolveContradiction cl f@(FForall x g) = do
    -- Reemplazo la variable por una metavariable así unificando encontramos la
    -- sustitución que permite refutarla (en caso de que exista)
    let gMeta = subst x TMetavar g
    let clMeta = fromClause $ replaceFirst cl f gMeta

    -- Convierto la nueva cláusula a DNF
    let (dnfClMeta, _) = dnf ("h", clMeta) -- No importa la demo
    wrapR
        (printf "solving clause with metavar in dnf '%s'" (show dnfClMeta))
        (solveContradictionUnifying SSEmpty dnfClMeta)

-- solveContradictionUnifying encuentra la sustitución que hace a la fórmula
-- refutable (en caso de que exista).
-- Asume que la fórmula está en DNF.
solveContradictionUnifying :: SingleSubst -> Form -> Result SingleSubst
solveContradictionUnifying s (FOr l r) = do
    sL <- solveContradictionUnifying s l
    solveContradictionUnifying sL r
solveContradictionUnifying s i = solveClauseUnifying s i

-- Encuentra la sustitución que permite una contradicción: dos literales
-- opuestos que unifican o false.
solveClauseUnifying :: SingleSubst -> Form -> Result SingleSubst
solveClauseUnifying s rawClause = do
    clause <- toClause rawClause
    -- Contradicción por false
    if FFalse `elem` clause
        then return SSEmpty
        else -- No hay por false, buscamos dos opuestas que unifiquen
        case find isJust (map (findFirstUnifyingWithOpposite s clause) clause) of
            Just (Just (f, fNot, s)) -> return s
            Nothing -> Left "no opposites that unify"

findFirstUnifyingWithOpposite :: SingleSubst -> [Form] -> Form -> Maybe (Form, Form, SingleSubst)
findFirstUnifyingWithOpposite s (f' : fs) f = case unifyF s (FNot f) f' of
    Left _ -> findFirstUnifyingWithOpposite s fs f
    Right s -> Just (f, f', s)
findFirstUnifyingWithOpposite _ [] _ = Nothing

replaceFirst :: (Eq a) => [a] -> a -> a -> [a]
replaceFirst (x : xs) old new
    | x == old = new : xs
    | otherwise = x : replaceFirst xs old new
replaceFirst [] _ _ = []

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
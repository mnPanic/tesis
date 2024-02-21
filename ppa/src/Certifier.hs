module Certifier (
    dnf,
    solve,
    toClause,
    fromClause,
    certifyBy,
    fromDNF,
    findContradiction,
) where

import PPA (
    Context,
    Decl (..),
    Hypothesis (HAxiom, HTheorem),
    Justification,
    Program,
    TProof,
    findHyp,
    getForm,
 )

import NDProofs (
    EnvItem,
    Result,
    cut,
    doubleNegElim,
    hypForm,
    proofAndAssoc,
    proofAndCongruence1,
    proofAndCongruence2,
    proofAndDistOverOrL,
    proofAndDistOverOrR,
    proofAndEProjection,
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
 )

import ND (
    Form (..),
    Proof (..),
    dneg,
 )

import Data.List (find)
import Data.Maybe (fromJust, isNothing)
import Text.Printf (printf)

check :: Context -> Bool
check = undefined

certify :: Program -> Result Context
certify = certify' []

certify' :: Context -> Program -> Result Context
certify' ctx (d : ds) = do
    dCtx <- certifyDecl ctx d
    certify' dCtx ds

certifyDecl :: Context -> Decl -> Result Context
certifyDecl ctx d@(DAxiom{}) = certifyAxiom ctx d
certifyDecl ctx d@(DTheorem{}) = certifyTheorem ctx d

certifyAxiom :: Context -> Decl -> Result Context
certifyAxiom ctx (DAxiom h f) = return (HAxiom h f : ctx)

certifyTheorem :: Context -> Decl -> Result Context
certifyTheorem ctx (DTheorem h f p) = do
    ndProof <- certifyProof ctx f p
    return (HTheorem h f ndProof : ctx)

certifyProof :: Context -> Form -> TProof -> Result Proof
certifyProof = undefined

{-
execute :: Program -> CheckResult
execute (ProgramT t) = checkT t

checkT :: Theorem -> CheckResult
checkT (Theorem _ f p) = check EEmpty (checkP p f) f

-- TODO: Hay que bancar diffuse reasoning
checkP :: TProof -> Form -> Proof
checkP (p : ps) thesis = checkPS p thesis ps

checkPS :: ProofStep -> Form -> TProof -> Proof
checkPS (PSAssume name form) (FImp f1 f2) ps = PImpI name (checkP ps f2)
checkPS (PSThus form hyp) thesis _
    | form /= thesis = error "form not thesis"
    | otherwise = PAx hyp

-}

{- Certifica que js => f

Si las justificaciones son [h1, .., hn] y el contexto tiene {h1: f1, ... hn:
fn}, da una demostración de que

    (f1 ^ ... ^ fn) => f
-}
certifyBy :: Context -> Form -> Justification -> Result Proof
certifyBy ctx f js = do
    justHyps <- findJustification ctx js
    let justForms = map getForm justHyps

    let thesis = FImp (fromClause justForms) f
    let fNotThesis = FNot thesis
    let hNotThesis = hypForm fNotThesis

    let (fDNFNotThesis, dnfProof) = dnf (hNotThesis, fNotThesis)

    let hDNFNotThesis = hypForm fDNFNotThesis

    contradictionProof <- solve (hDNFNotThesis, fDNFNotThesis)
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

findJustification :: Context -> Justification -> Result [Hypothesis]
findJustification ctx js
    | not (null missingHyps) =
        Left
            $ "justifications not present in context: "
            ++ show (map fst missingHyps)
    | otherwise = Right (map (fromJust . snd) hyps)
  where
    hyps = zip js $ map (findHyp ctx) js
    missingHyps = filter (\(h, r) -> isNothing r) hyps

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

{- solve demuestra una contradicción de una fórmula que se asume que está en
DNF. Para ello refuta cada cláusula, buscando o el mismo literal negado y sin
negar, o que tenga false.

Devuelve una demostración de
    f |- bot
-}
solve :: EnvItem -> Result Proof
solve (hOr, FOr l r) = do
    let hLeft = hOr ++ " L"
    let hRight = hOr ++ " R"
    proofLeft <- solve (hLeft, l)
    proofRight <- solve (hRight, r)
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
solve i = solveClause i

{- solveClause intenta demostrar que una cláusula es contradictoria.

    p1 ^ p2 ^ ... ^ p_n |- bot

busca el mismo literal opuesto, o que alguno sea false.
Asume que la fórmula es una cláusula: Una conjunción de literales (predicados,
true y false)
-}
solveClause :: EnvItem -> Result Proof
solveClause (h, rawClause) = do
    clause <- toClause rawClause
    contradictingForm <- findContradiction clause
    case contradictingForm of
        FFalse -> do
            proofFalse <- proofAndEProjection (h, rawClause) FFalse
            return
                ( PNamed
                    (printf "contradiction of %s by false" (show rawClause))
                    proofFalse
                )
        f -> do
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

-- Clause es una conjunción de literales
type Clause = [Form]

-- Encuentra dos literales opuestos o false. Devuelve una sola fórmula, que es o
-- bien false o una que se contradice con su negación.
findContradiction :: Clause -> Result Form
findContradiction fs
    -- Contradicción por false
    | FFalse `elem` fs = Right FFalse
    -- No hay por false, buscamos dos opuestas
    | otherwise = case filter hasOpposite fs of
        (f : _) -> Right f
        [] -> Left $ printf "%s contains no contradicting literals or false" (show fs)
  where
    hasOpposite f = FNot f `elem` fs

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
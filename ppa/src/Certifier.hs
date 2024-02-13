module Certifier (
    dnf,
    solve,
    toClause,
    fromClause,
    fromDNF,
    findContradiction,
) where

import PPA (
    Context,
    Program,
    TProof,
 )

import NDProofs (
    EnvItem,
    Result,
    hypForm,
    proofAndCongruence,
    proofAndEProjection,
    proofImpElim,
 )

import ND (
    Form (..),
    Proof (..),
 )

import Data.List (find)
import Text.Printf (printf)

certify :: Program -> Context
certify = undefined

check :: Context -> Bool
check = undefined

type M = Maybe

certifyProof :: Context -> Form -> TProof -> M Proof
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

{- dnf

Dada una fórmula F y una versión en DNF F' (no es única), da una demostración de
F |- F'.
-}
dnf :: Form -> Result (Form, Proof)
dnf f = Right (f, PAx "h")

convertToDnf :: Form -> (Form, Proof)
convertToDnf f = undefined

{- dnfStep hace una transformación "small-step" de una fórmula hacia DNF.
Dada una fórmula F, devuelve la fórmula F' con un paso aplicado, y las
demostraciones de F |- F' y F' |- F (necesarias para la congruencia, dado que
algunos operadores como el Not son opuestos).

Devuelve Nothing cuando F ya está en DNF.
-}
-- TODO: capaz las hip las tiene que devolver acá
dnfStep :: Form -> Maybe (Form, Proof, Proof)
-- Casos de reescritura
dnfStep fImp@(FImp a b) = Just (fOr, pImpElim, pOrToImp)
  where
    fOr = FOr (FNot a) b
    (pImpElim, pOrToImp) = proofImpElim a b (hypForm fImp) (hypForm fOr)

-- Casos de congruencia
dnfStep (FAnd l r) = case dnfStep l of
    -- l se puede reescribir
    Just (l', pLToL', _) -> proofAndCongruence
    Nothing -> undefined

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
        FFalse -> proofAndEProjection (h, rawClause) FFalse
        f -> do
            proofF <- proofAndEProjection (h, rawClause) f
            proofNotF <- proofAndEProjection (h, rawClause) (FNot f)
            return
                PNotE
                    { form = f
                    , proofNotForm = proofNotF
                    , proofForm = proofF
                    }

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

isLiteral :: Form -> Bool
isLiteral (FNot f) = isLiteral f && f /= FTrue && f /= FFalse
isLiteral (FPred _ _) = True
isLiteral FTrue = True
isLiteral FFalse = True
isLiteral (FForall _ _) = True
isLiteral (FExists _ _) = True
isLiteral _ = False
module PPA (
  TProof,
  ProofStep (..),
  Program (..),
  Context (..),
  Justification,
  Hypothesis (..),
  Decl (..),
  findHyp,
  getHypId,
  getProof,
  getForm,
) where

import Data.List (find)
import ND (Env (EEmpty), Form (..), HypId, Proof (..))
import NDChecker (
  CheckResult,
  check,
 )
import NDProofs (Result)
import Text.Printf (printf)

type Program = [Decl]

data Decl
  = DAxiom HypId Form
  | DTheorem HypId Form TProof
  deriving (Show, Eq)

type TProof = [ProofStep]

data ProofStep
  = PSSuppose HypId Form
  | -- Thus prueba algo de la tesis
    PSThusBy Form Justification
  | -- Have prueba algo auxiliar
    PSHaveBy HypId Form Justification
  | -- Tesis es equivalente a otra fórmula
    PSEquiv Form
  | -- Afirmación auxiliar con su demostración
    PSClaim HypId Form TProof
  deriving (Show, Eq)

type Justification = [HypId]

type Context = [Hypothesis]

-- TODO: No se usa?
type Goal = (Context, Form)

data Hypothesis
  = HAxiom HypId Form
  | HTheorem HypId Form Proof
  deriving (Show, Eq)

getHypId :: Hypothesis -> HypId
getHypId (HAxiom h _) = h
getHypId (HTheorem h _ _) = h

getForm :: Hypothesis -> Form
getForm (HAxiom _ f) = f
getForm (HTheorem _ f _) = f

getProof :: Hypothesis -> Proof
getProof (HAxiom h _) = PAx h
getProof (HTheorem _ _ p) = p

findHyp :: Context -> HypId -> Result Hypothesis
findHyp ctx h
  | h == prevHypId = do
      prev <- getPrevHypId ctx
      findHyp ctx prev
  | otherwise = case find (\h' -> getHypId h' == h) ctx of
      Just hyp -> Right hyp
      Nothing -> Left $ printf "'%s' not present in ctx" h

-- hypId especial que se refiere a la anterior
prevHypId :: String
prevHypId = "-"

-- Devuelve la última hipótesis que se agregó al contexto
getPrevHypId :: Context -> Result HypId
getPrevHypId ctx
  | null ctx = Left "can't get prev hyp from empty ctx"
  | otherwise = return $ getHypId $ head ctx

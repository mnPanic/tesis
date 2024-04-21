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
  | -- Hence es como thus, pero agregando la hipótesis anterior
    PSHenceBy Form Justification
  | -- Have prueba algo auxiliar
    PSHaveBy HypId Form Justification
  | -- Then es como have, pero agregando la hipótesis anterior
    PSThenBy HypId Form Justification
  | -- Tesis es equivalente a otra fórmula
    PSEquiv Form
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

findHyp :: Context -> HypId -> Maybe Hypothesis
findHyp ctx h = find (\h' -> getHypId h' == h) ctx
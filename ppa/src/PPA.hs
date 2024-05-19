module PPA (
  TProof,
  ProofStep (..),
  Program (..),
  Context (..),
  Justification,
  Hypothesis (..),
  Decl (..),
  Case,
  findHyp,
  getHypId,
  getProof,
  getForm,
  fvC,
) where

import Data.List (find)
import ND (
  Env (EEmpty),
  Form (..),
  HypId,
  Proof (..),
  Term,
  VarId,
  fv,
 )
import NDChecker (
  CheckResult,
  check,
 )
import NDProofs (Result)
import Text.Printf (printf)

import Data.Set qualified as Set

type Program = [Decl]

data Decl
  = DAxiom HypId Form
  | DTheorem HypId Form TProof
  deriving (Show, Eq)

type TProof = [ProofStep]

type Case = (HypId, Form, TProof)

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
  | PSCases Justification [Case]
  | PSTake VarId Term
  | PSConsider VarId HypId Form Justification
  | PSLet VarId VarId
  deriving (Show, Eq)

type Justification = [HypId]

type Context = [Hypothesis]

-- TODO: No se usa?
type Goal = (Context, Form)

data Hypothesis
  = HAxiom HypId Form
  | HTheorem HypId Form Proof
  deriving (Eq)

instance Show Hypothesis where
  show (HAxiom h f) = printf "(axiom) %s : %s" h (show f)
  show (HTheorem h f p) = printf "(theorem) %s : %s . Proof = %s" h (show f) (show p)

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
  | h == prevHypId = getPrevHyp ctx
  | otherwise = case find (\h' -> getHypId h' == h) ctx of
      Just hyp -> Right hyp
      Nothing -> Left $ printf "'%s' not present in ctx" h

-- hypId especial que se refiere a la anterior
prevHypId :: String
prevHypId = "-"

-- Devuelve la última hipótesis que se agregó al contexto
getPrevHyp :: Context -> Result Hypothesis
getPrevHyp ctx
  | null ctx = Left "can't get prev hyp from empty ctx"
  | otherwise = return $ head ctx

fvC :: Context -> Set.Set VarId
fvC = foldr (Set.union . fv . getForm) Set.empty
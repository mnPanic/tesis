module PPA.PPA (
  TProof,
  ProofStep (..),
  Program,
  Context,
  Justification,
  Hypothesis (..),
  Decl (..),
  Case,
  findHyp,
  axioms,
  removeHyp,
  getHypId,
  sizeC,
  getProof,
  getForm,
  fvC,
  psName,
) where

import Data.List (find)
import ND.ND (
  Form (..),
  HypId,
  Proof (..),
  Term,
  VarId,
  fv,
  heightP,
 )

import Result (Result)
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
  | PSLet VarId
  deriving (Show, Eq)

psName :: ProofStep -> String
psName ps = case ps of
  (PSSuppose{}) -> "suppose"
  (PSThusBy{}) -> "thus"
  (PSHaveBy h _ _) -> printf "have (%s)" h
  (PSEquiv{}) -> "equiv"
  (PSClaim{}) -> "claim"
  (PSCases{}) -> "cases"
  (PSTake{}) -> "take"
  (PSConsider{}) -> "consider"
  (PSLet{}) -> "let"

type Justification = [HypId]

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

isAxiom :: Hypothesis -> Bool
isAxiom HAxiom{} = True
isAxiom HTheorem{} = False

type Context = [Hypothesis]

sizeC :: Context -> Int
sizeC = sum . map sizeH
 where
  sizeH h = case h of
    HAxiom{} -> 1
    HTheorem _ _ p -> heightP p

findHyp :: Context -> HypId -> Result Hypothesis
findHyp ctx h
  | h == prevHypId = getPrevHyp ctx
  | otherwise = case find (\h' -> getHypId h' == h) ctx of
      Just hyp -> Right hyp
      Nothing -> Left $ printf "'%s' not present in ctx" h

removeHyp :: Context -> HypId -> Context
removeHyp ctx hypId = filter (\h -> getHypId h /= hypId) ctx

axioms :: Context -> Context
axioms = filter isAxiom

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

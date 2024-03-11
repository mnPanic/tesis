module PPA (
    TProof,
    ProofStep (..),
    Program (..),
    Context (..),
    Justification,
    Hypothesis (..),
    Decl (..),
    findHyp,
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
    = PSAssume HypId Form
    | PSThusBy Form Justification
    | PSThenBy Form HypId Justification -- TODO revisar
    deriving (Show, Eq)

type Justification = [HypId]

type Context = [Hypothesis]

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

findHyp :: Context -> HypId -> Maybe Hypothesis
findHyp ctx h = find (\h' -> getHypId h' == h) ctx
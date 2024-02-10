module PPA (
    TProof,
    ProofStep (..),
    Program (..),
    Context (..),
) where

import ND (Env (EEmpty), Form (..), HypId, Proof (..))
import NDChecker (
    CheckResult,
    check,
 )

type Program = [Decl]

data Decl
    = DAxiom HypId Form
    | DTheorem HypId Form TProof
    deriving (Show)

type TProof = [ProofStep]

data ProofStep
    = PSAssume String Form
    | PSThusBy Form Justification
    | PSThenBy Form HypId Justification -- TODO revisar
    deriving (Show)

type Justification = [HypId]

type Context = [Hypothesis]

type Goal = (Context, Form)

data Hypothesis
    = HAxiom HypId Form
    | HTheorem HypId Form Proof

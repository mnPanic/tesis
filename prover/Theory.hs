module Theory (
    Theorem (..),
    Proof,
    ProofStep (..),
    Program (..),
) where

import Prover (Form)

data Program
    = ProgramT Theorem
    | ProgramF Form
    deriving (Show)

data Theorem = Theorem String Form Proof
    deriving (Show)

type Proof = [ProofStep]

data ProofStep
    = PSAssume String Form
    | PSThus Form String
    deriving (Show)
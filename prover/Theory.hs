module Theory (
    Theorem (..),
    TProof,
    ProofStep (..),
    Program (..),
    execute,
) where

import Prover (CheckResult, Env (EEmpty), Form (..), Proof (..), check)

data Program
    = ProgramT Theorem
    | ProgramF Prover.Form
    deriving (Show)

data Theorem = Theorem String Form TProof
    deriving (Show)

type TProof = [ProofStep]

data ProofStep
    = PSAssume String Form
    | PSThus Form String
    deriving (Show)

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
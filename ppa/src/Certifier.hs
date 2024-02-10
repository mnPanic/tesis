module Certifier () where

import PPA (
    Context,
    Program,
    TProof,
 )

import ND (
    Form (..),
    Proof (..),
 )

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
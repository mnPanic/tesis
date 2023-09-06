module Prover (
               Env(..), get,
               VarId, FunId, PredId, HypId,
               Term(..), Form(..), Proof(..),
               check, CheckResult(..)
               ) where

import Text.Printf

-- Tipos de identificadores
type VarId = String
type FunId = String
type PredId = String
type HypId = String

data Term =
    TVar VarId
    | TFun FunId [Term]
    deriving (Show, Eq)

data Form =
    FPred PredId [Term]
    | FAnd Form Form
    | FOr Form Form
    | FImp Form Form
    | FNot Form
    | FTrue
    | FFalse
    | FForall VarId Form
    | FExists VarId Form
    deriving (Show, Eq)

data Env = EEmpty
    | EExtend HypId Form Env
    deriving (Show, Eq)

get :: Env -> HypId -> Maybe Form
get EEmpty hyp = Nothing
get (EExtend hyp' f env) hyp
    | hyp == hyp' = Just f
    | otherwise = get env hyp

data Proof =
    PAx HypId
    -- A ^ B
    | PAndI Proof -- A
            Proof -- de B
    | PAndE1 Form -- B
             Proof -- de A
    | PAndE2 Form -- A
             Proof -- de B
    -- A v B deduce C
    | POrI1 Proof -- A
    | POrI2 Proof -- B
    | POrE Form -- A
           Form -- B
           Proof -- de A v B
           HypId -- x:A
           Proof -- de C asumiendo A
           HypId -- x:B
           Proof -- de C asumiendo B
    -- A -> B
    | PImpI HypId Proof
    | PImpE Form -- A
            Proof -- de A -> B
            Proof -- de A
    -- ¬ A = A -> bottom
    | PNotI HypId -- x:A
            Proof -- de bottom
    | PNotE Form -- A
            Proof -- de ~A
            Proof -- de A
    | PTrueI
    | PFalseE Proof
    | PLEM
    -- V x . A
    | PForallI Proof -- de A
    | PForallE VarId -- x
               Form -- A (sin sust)
               Proof -- de V x . A
    -- E x . A deduce B
    | PExistsI Term -- t
               Proof -- de A con x reemplazado por t  
    | PExistsE VarId -- x:A
               Form -- A
               Proof -- de E x. A
    deriving (Show, Eq)

data CheckResult = CheckOK
    | CheckError Env Proof Form String
    deriving (Show, Eq)

{- TODO:
    - Mónadas para chaining de errores? Por ej en PImpE
    - En vez de tomar la form por pattern matching directo, tomar la form y
      hacer un case para poder retornar error en vez de un non exhaustive cuando
      use mal una regla?
-}
check :: Env -> Proof -> Form -> CheckResult

-- TODO: handlearlo cuando es al reves, not A v A que es lo mismo
check env (PLEM) (FOr f1 (FNot f2)) =
    if f1 == f2
    then CheckOK
    else CheckError env (PLEM) (FOr (FNot f1) f2) "LEM proves A v ~A for the same form A, but they are different"

check env (PAx hyp) f =
    case get env hyp of
        Just f' -> if f == f'
                   then CheckOK
                   else CheckError env (PAx hyp) f (printf "env has hyp %s for different form" hyp)
        Nothing -> CheckError env (PAx hyp) f (printf "hyp %s not in env" hyp)

-- no importa quien es f, false demuestra cualquier cosa
check env (PFalseE pFalse) _ = check env pFalse FFalse

-- dem A -> B
check env (PImpI hyp proofB) (FImp fA fB) =
    check (EExtend hyp fA env) proofB fB

-- dem B con A -> B
check env (PImpE fA proofAImpB proofA) fB =
    case check env proofAImpB (FImp fA fB) of
        CheckError e p f s -> CheckError e p f s
        CheckOK -> check env proofA fA

-- dem not A
check env (PNotI hyp proofFalse) (FNot f) =
    check (EExtend hyp f env) proofFalse FFalse

-- dem False con not A
check env (PNotE fA proofNotA proofA) FFalse =
    case check env proofNotA (FNot fA) of
        CheckError e p f s -> CheckError e p f s
        CheckOK -> check env proofA fA

-- dem C con A v B
check env (POrE fA fB proofAorB hypA proofAC hypB proofBC) fC =
    case check env proofAorB (FOr fA fB) of
        CheckError e p f s -> CheckError e p f s
        CheckOK -> case check (EExtend hypA fA env) proofAC fC of
            CheckError e p f s -> CheckError e p f s
            CheckOK -> check (EExtend hypB fB env) proofBC fC

-- Error para agarrar todo lo no handleado
check env proof form = CheckError env proof form "Unhandled proof"
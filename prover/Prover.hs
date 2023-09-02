module Prover where

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
    | PNotE Form
            Proof
            Proof
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

check :: Env -> Proof -> Form -> CheckResult
check env (PAx hyp) f =
    case (get env hyp) of
        Just f' -> if f == f' then CheckOK
                   else CheckError
                        env (PAx hyp) f
                        (printf "env has hyp %s for different form" hyp)
        Nothing -> CheckError env (PAx hyp) f (printf "hyp %s not in env" hyp)
check env (PImpI hyp proof) (FImp f1 f2) = check (EExtend hyp f1 env) proof f2


module Prover (
    Env(..), get,
    VarId, FunId, PredId, HypId,
    Term(..), Form(..), Proof(..),
    check, CheckResult(..), subst, fv, fvE
    ) where

import Text.Printf ( printf )
import qualified Data.Set as Set

-- Tipos de identificadores
type VarId = String
type FunId = String
type PredId = String
type HypId = String

data Term =
    TVar VarId
    | TFun FunId [Term]
    deriving (Show, Eq)

-- Free variables de un término
fvT :: Term -> Set.Set VarId
fvT (TVar x) = Set.singleton x
fvT (TFun _ ts) = foldr (Set.union . fvT) Set.empty ts

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

fv :: Form -> Set.Set VarId
fv (FPred _ ts) = foldr (Set.union . fvT) Set.empty ts
fv (FAnd f1 f2) = Set.union (fv f1) (fv f2)
fv (FOr f1 f2) = Set.union (fv f1) (fv f2)
fv (FImp f1 f2) = Set.union (fv f1) (fv f2)
fv (FNot f1) = fv f1
fv FTrue = Set.empty
fv FFalse = Set.empty
fv (FForall y f1) = Set.delete y (fv f1)
fv (FExists y f1) = Set.delete y (fv f1)

data Env = EEmpty
    | EExtend HypId Form Env
    deriving (Show, Eq)

get :: Env -> HypId -> Maybe Form
get EEmpty hyp = Nothing
get (EExtend hyp' f env) hyp
    | hyp == hyp' = Just f
    | otherwise = get env hyp

forms :: Env -> [Form]
forms EEmpty = []
forms (EExtend _ f e') = f:forms e'

fvE :: Env -> Set.Set VarId
fvE e = foldr (Set.union . fv) Set.empty (forms e)

-- substituye todas las ocurrencias libres de VarId por Term
subst :: VarId -> Term -> Form -> Form
subst x t f = case f of
    FPred l ts -> FPred l (map (substTerm x t) ts)
    FAnd f1 f2 -> FAnd (rec f1) (rec f2)
    FOr f1 f2 -> FOr (rec f1) (rec f2)
    FImp f1 f2 -> FImp (rec f1) (rec f2)
    FNot f1 -> FNot (rec f1)
    FTrue -> FTrue
    FFalse -> FFalse
    -- si y esta libre en T
    -- buscar y' que no este libre en T, f1 dif de y.
    -- en f1 y por y'
    -- reemplazar y' recursivamente por T
    orig@(FForall y f1) -> if x == y 
                           then orig
                           else FForall y (rec f1)
    orig@(FExists y f1) -> if x == y 
                           then orig
                           else FExists y (rec f1)

    where rec = subst x t

substTerm :: VarId -> Term -> Term -> Term
substTerm x t t' = case t' of 
    o@(TVar y) -> if x == y then t else o
    TFun f ts -> TFun f (map (substTerm x t) ts)

data Proof =
    PAx HypId
    -- A ^ B
    | PAndI Proof -- A
            Proof -- de B
    | PAndE1 Form -- B
             Proof -- de A ^ B
    | PAndE2 Form -- A
             Proof -- de A ^ B
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
    -- También conocida como RAA (reducción al absurdo)
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
               Term -- t
    -- E x . A
    | PExistsI Term -- t
               Proof -- de A con x reemplazado por t  
    -- E x . A deduce B
    | PExistsE VarId -- x
               Form -- A
               Proof -- de E x. A
               HypId -- x:A
               Proof -- de B con A como hyp
    deriving (Show, Eq)

data CheckResult = CheckOK
    | CheckError Env Proof Form String
    deriving (Eq)

instance Show CheckResult where
    show CheckOK = "OK"
    show (CheckError env proof form msg) =
        printf "Check error! %s on\nenv: %s\nproof: %s\nform: %s\n"
            msg
            (show env)
            (show proof)
            (show form)

{- TODO:
    - Mónadas para chaining de errores? Por ej en PImpE
-}
check :: Env -> Proof -> Form -> CheckResult

-- TODO: handlearlo cuando es al reves, not A v A que es lo mismo
check env PLEM form@(FOr f1 (FNot f2)) =
    if f1 == f2
    then CheckOK
    else CheckError env PLEM form "LEM proves A v ~A for the same form A, but they are different"

check env proof@(PAx hyp) f =
    case get env hyp of
        Just f' -> if f == f'
                   then CheckOK
                   else CheckError env proof f (printf "env has hyp %s for different form" hyp)
        Nothing -> CheckError env proof f (printf "hyp %s not in env" hyp)

-- no importa quien es f, false demuestra cualquier cosa
check env (PFalseE pFalse) _ = check env pFalse FFalse

check env PTrueI FTrue = CheckOK

-- dem A -> B
check env (PImpI hyp proofB) (FImp fA fB) =
    check (EExtend hyp fA env) proofB fB

-- dem B con A -> B
check env (PImpE fA proofAImpB proofA) fB =
    case check env proofAImpB (FImp fA fB) of
        err@(CheckError {}) -> err
        CheckOK -> check env proofA fA

-- dem not A
check env (PNotI hyp proofFalse) (FNot f) =
    check (EExtend hyp f env) proofFalse FFalse

-- dem False con not A
check env (PNotE fA proofNotA proofA) FFalse =
    case check env proofNotA (FNot fA) of
        err@(CheckError {}) -> err
        CheckOK -> check env proofA fA

-- dem C con A v B
check env (POrE fA fB proofAorB hypA proofAC hypB proofBC) fC =
    case check env proofAorB (FOr fA fB) of
        err@(CheckError {}) -> err
        CheckOK -> case check (EExtend hypA fA env) proofAC fC of
            err@(CheckError {}) -> err
            CheckOK -> check (EExtend hypB fB env) proofBC fC

-- dem A v B
check env (POrI1 proofA) (FOr fA _) = check env proofA fA
check env (POrI2 proofB) (FOr _ fB) = check env proofB fB

-- dem con A ^ B
check env (PAndE1 fB proofAnB) fA = check env proofAnB (FAnd fA fB) 
check env (PAndE2 fA proofAnB) fB = check env proofAnB (FAnd fA fB)

-- dem de A ^ B
check env (PAndI proofA proofB) (FAnd fA fB) =
    case check env proofA fA of
        err@(CheckError {}) -> err
        CheckOK -> check env proofB fB

-- dem de Exists x. A, prueba A{x := t}
check env (PExistsI t proofSubstA) (FExists x f) =
    check env proofSubstA (subst x t f)

-- del de B con Exists x. A
check env proof@(PExistsE x fA proofExistsxA hypA proofB) fB
    | x `elem` fvE env = CheckError env proof fB (printf "env shouldn't contain fv '%s'" x)
    | x `elem` fv fB = CheckError env proof fB (printf "form to prove shoudln't contain fv '%s'" x)
    | otherwise = case check env proofExistsxA (FExists x fA) of
            err@(CheckError {}) -> err
            CheckOK -> check (EExtend hypA fA env) proofB fB

-- dem de Forall x. A
check env proof@(PForallI proofA) form@(FForall x fA) = 
    if x `elem` fvE env
    then CheckError env proof form (printf "env shouldn't contain fv '%s'" x)
    else check env proofA fA

-- dem de A{x:=t} usando Forall x. A
check env proof@(PForallE x fA proofForallxA t) fAxt =
    if subst x t fA /= fAxt
    then CheckError
            env proof fAxt
            (printf "form %s /= (%s){%s := %s}" 
                (show fAxt)
                (show fA)
                x
                (show t))
    else check env proofForallxA (FForall x fA)

-- Error para agarrar todo lo no handleado
check env proof form = CheckError env proof form "Unhandled proof"
module Prover (
    Env (..),
    get,
    VarId,
    FunId,
    PredId,
    HypId,
    Term (..),
    Form (..),
    Proof (..),
    check,
    CheckResult (..),
    subst,
    fv,
    fvE,
) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Text.Printf (printf)

-- Tipos de identificadores
type VarId = String
type FunId = String
type PredId = String
type HypId = String

type Subst = Map.Map String String

data Term
    = TVar VarId
    | TFun FunId [Term]
    deriving (Show)

-- Free variables de un término
fvTerm :: Term -> Set.Set VarId
fvTerm (TVar x) = Set.singleton x
fvTerm (TFun _ ts) = foldr (Set.union . fvTerm) Set.empty ts

instance Eq Term where
    (==) = alphaEqTerm Map.empty Map.empty

alphaEqTerm :: Subst -> Subst -> Term -> Term -> Bool
alphaEqTerm m1 m2 (TVar x) (TVar y)
    | x == y = True
    | otherwise = case Map.lookup x m1 of
        Nothing -> False
        Just x' -> case Map.lookup y m2 of
            Nothing -> False
            Just y' -> x' == y'
alphaEqTerm m1 m2 (TFun f1 ts1) (TFun f2 ts2) = f1 == f2 && alphaEqTerms m1 m2 ts1 ts2
alphaEqTerm _ _ _ _ = False -- Diferente forma

alphaEqTerms :: Subst -> Subst -> [Term] -> [Term] -> Bool
alphaEqTerms m1 m2 ts1 ts2 =
    length ts1
        == length ts2
        && and (zipWith (alphaEqTerm m1 m2) ts1 ts2)

data Form
    = FPred PredId [Term]
    | FAnd Form Form
    | FOr Form Form
    | FImp Form Form
    | FNot Form
    | FTrue
    | FFalse
    | FForall VarId Form
    | FExists VarId Form
    deriving (Show)

instance Eq Form where
    (==) = alphaEqForm' Map.empty Map.empty

alphaEqForm' :: Subst -> Subst -> Form -> Form -> Bool
alphaEqForm' = alphaEqForm 0

-- alphaEqForm chequea que dos fórmulas sean alpha equivalentes.
-- Por ej
--
--  E x . f(x) `alphaEq` E y . f(y)
--
-- Cuando se encuentra con una variable ligada, si son diferentes se mapean a
-- una variable libre fresca igual para ambos. Si es la misma se sigue de largo.
-- Luego, para ver si una var es igual a otra, nos fijamos que tengan mapeado el
-- mismo nombre.
alphaEqForm :: Int -> Subst -> Subst -> Form -> Form -> Bool
alphaEqForm n _ _ FTrue FTrue = True
alphaEqForm n _ _ FFalse FFalse = True
alphaEqForm n m1 m2 (FPred p1 ts1) (FPred p2 ts2) = p1 == p2 && alphaEqTerms m1 m2 ts1 ts2
alphaEqForm n m1 m2 (FAnd f1 g1) (FAnd f2 g2) = alphaEqForm n m1 m2 f1 f2 && alphaEqForm n m1 m2 g1 g2
alphaEqForm n m1 m2 (FOr f1 g1) (FOr f2 g2) = alphaEqForm n m1 m2 f1 f2 && alphaEqForm n m1 m2 g1 g2
alphaEqForm n m1 m2 (FImp f1 g1) (FImp f2 g2) = alphaEqForm n m1 m2 f1 f2 && alphaEqForm n m1 m2 g1 g2
alphaEqForm n m1 m2 (FNot f1) (FNot f2) = alphaEqForm n m1 m2 f1 f2
alphaEqForm n m1 m2 (FForall x f1) (FForall y f2)
    | x == y = alphaEqForm n m1 m2 f1 f2
    | otherwise =
        alphaEqForm
            (n + 1)
            (Map.insert x (varN n) m1)
            (Map.insert y (varN n) m2)
            f1
            f2
alphaEqForm n m1 m2 (FExists x f1) (FExists y f2)
    | x == y = alphaEqForm n m1 m2 f1 f2
    | otherwise =
        alphaEqForm
            (n + 1)
            (Map.insert x (varN n) m1)
            (Map.insert y (varN n) m2)
            f1
            f2
alphaEqForm _ _ _ _ _ = False -- Diferente forma

-- Devuelve la n-esima variable
varN :: Int -> VarId
varN n = "s_" ++ show n

fv :: Form -> Set.Set VarId
fv (FPred _ ts) = foldr (Set.union . fvTerm) Set.empty ts
fv (FAnd f1 f2) = Set.union (fv f1) (fv f2)
fv (FOr f1 f2) = Set.union (fv f1) (fv f2)
fv (FImp f1 f2) = Set.union (fv f1) (fv f2)
fv (FNot f1) = fv f1
fv FTrue = Set.empty
fv FFalse = Set.empty
fv (FForall y f1) = Set.delete y (fv f1)
fv (FExists y f1) = Set.delete y (fv f1)

data Env
    = EEmpty
    | EExtend HypId Form Env
    deriving (Show, Eq)

get :: Env -> HypId -> Maybe Form
get EEmpty hyp = Nothing
get (EExtend hyp' f env) hyp
    | hyp == hyp' = Just f
    | otherwise = get env hyp

forms :: Env -> [Form]
forms EEmpty = []
forms (EExtend _ f e') = f : forms e'

fvE :: Env -> Set.Set VarId
fvE e = foldr (Set.union . fv) Set.empty (forms e)

-- substituye todas las ocurrencias libres de VarId por Term
-- Lo hace alpha-renombrando en el camino: si y esta libre en T, busca y' que no
-- este libre en T y f1. Reemplaza en f1 recursivamente y por y' y continua.
subst :: VarId -> Term -> Form -> Form
subst x t f = case f of
    FPred l ts -> FPred l (map (substTerm x t) ts)
    FAnd f1 f2 -> FAnd (rec f1) (rec f2)
    FOr f1 f2 -> FOr (rec f1) (rec f2)
    FImp f1 f2 -> FImp (rec f1) (rec f2)
    FNot f1 -> FNot (rec f1)
    FTrue -> FTrue
    FFalse -> FFalse
    orig@(FForall y f1)
        -- No está libre, no cambio
        | x == y -> orig
        -- Captura de variables
        | y `elem` fvTerm t -> FForall y' (rec (subst y (TVar y') f1))
        | otherwise -> FForall y (rec f1)
      where
        y' = freshWRT y $ fv f1 `Set.union` fvTerm t
    orig@(FExists y f1)
        -- No está libre, no cambio
        | x == y -> orig
        -- Captura de variables
        | y `elem` fvTerm t -> FExists y' (rec (subst y (TVar y') f1))
        | otherwise -> FExists y (rec f1)
      where
        y' = freshWRT y $ fv f1 `Set.union` fvTerm t
  where
    rec = subst x t

substTerm :: VarId -> Term -> Term -> Term
substTerm x t t' = case t' of
    o@(TVar y) -> if x == y then t else o
    TFun f ts -> TFun f (map (substTerm x t) ts)

-- freshWRT da una variable libre con respecto a una lista en donde no queremos
-- que aparezca
freshWRT :: (Foldable t) => VarId -> t VarId -> VarId
freshWRT x forbidden = head [x ++ suffix | suffix <- map show [0 ..], x ++ suffix `notElem` forbidden]

data Proof
    = PAx HypId
    | -- A ^ B
      PAndI
        Proof -- A
        Proof -- de B
    | PAndE1
        Form -- B
        Proof -- de A ^ B
    | PAndE2
        Form -- A
        Proof -- de A ^ B
        -- A v B deduce C
    | POrI1 Proof -- A
    | POrI2 Proof -- B
    | POrE
        Form -- A
        Form -- B
        Proof -- de A v B
        HypId -- x:A
        Proof -- de C asumiendo A
        HypId -- x:B
        Proof -- de C asumiendo B
        -- A -> B
    | PImpI HypId Proof
    | PImpE
        Form -- A
        Proof -- de A -> B
        Proof -- de A
        -- ¬ A = A -> bottom
        -- También conocida como RAA (reducción al absurdo)
    | PNotI
        HypId -- x:A
        Proof -- de bottom
    | PNotE
        Form -- A
        Proof -- de ~A
        Proof -- de A
    | PTrueI
    | PFalseE Proof
    | PLEM
    | -- V x . A
      PForallI Proof -- de A
    | PForallE
        VarId -- x
        Form -- A (sin sust)
        Proof -- de V x . A
        Term -- t
        -- E x . A
    | PExistsI
        Term -- t
        Proof -- de A con x reemplazado por t
        -- E x . A deduce B
    | PExistsE
        VarId -- x
        Form -- A
        Proof -- de E x. A
        HypId -- x:A
        Proof -- de B con A como hyp
    deriving (Show, Eq)

data CheckResult
    = CheckOK
    | CheckError Env Proof Form String
    deriving (Eq)

instance Show CheckResult where
    show CheckOK = "OK"
    show (CheckError env proof form msg) =
        printf
            "Check error! %s on\nenv: %s\nproof: %s\nform: %s\n"
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
        Just f' ->
            if f == f'
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
        err@(CheckError{}) -> err
        CheckOK -> check env proofA fA
-- dem not A
check env (PNotI hyp proofFalse) (FNot f) =
    check (EExtend hyp f env) proofFalse FFalse
-- dem False con not A
check env (PNotE fA proofNotA proofA) FFalse =
    case check env proofNotA (FNot fA) of
        err@(CheckError{}) -> err
        CheckOK -> check env proofA fA
-- dem C con A v B
check env (POrE fA fB proofAorB hypA proofAC hypB proofBC) fC =
    case check env proofAorB (FOr fA fB) of
        err@(CheckError{}) -> err
        CheckOK -> case check (EExtend hypA fA env) proofAC fC of
            err@(CheckError{}) -> err
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
        err@(CheckError{}) -> err
        CheckOK -> check env proofB fB
-- dem de Exists x. A, prueba A{x := t}
check env (PExistsI t proofSubstA) (FExists x f) =
    check env proofSubstA (subst x t f)
-- del de B con Exists x. A
check env proof@(PExistsE x fA proofExistsxA hypA proofB) fB
    | x `elem` fvE env = CheckError env proof fB (printf "env shouldn't contain fv '%s'" x)
    | x `elem` fv fB = CheckError env proof fB (printf "form to prove shoudln't contain fv '%s'" x)
    | otherwise = case check env proofExistsxA (FExists x fA) of
        err@(CheckError{}) -> err
        CheckOK -> check (EExtend hypA fA env) proofB fB
-- dem de Forall x. A
check env proof@(PForallI proofA) form@(FForall x fA) =
    if x `elem` fvE env
        then CheckError env proof form (printf "env shouldn't contain fv '%s'" x)
        else check env proofA fA
-- dem de A{x:=t} usando Forall x. A
check env proof@(PForallE x fA proofForallxA t) fAxt =
    if subst x t fA /= fAxt
        then
            CheckError
                env
                proof
                fAxt
                ( printf
                    "form %s /= (%s){%s := %s}"
                    (show fAxt)
                    (show fA)
                    x
                    (show t)
                )
        else check env proofForallxA (FForall x fA)
-- Error para agarrar todo lo no handleado
check env proof form = CheckError env proof form "Unhandled proof"
{-# LANGUAGE ImportQualifiedPost #-}

-- El módulo ND (Natural Deduction) contiene tipos para demostraciones de
-- fórmulas de LPO con deducción natural.
module ND (
    Env (..),
    fromList,
    get,
    VarId,
    FunId,
    PredId,
    HypId,
    Term (..),
    Form (..),
    Proof (..),
    VarSubstitution,
    Metavar,
    fv,
    fvTerm,
    proofName,
    fvE,
    formsWithFv,
    fvP,
    propVar,
    predVar,
    fPred0,
    fPredVar,
    dneg,
    isForall,
    varN,
    tFun0,
    tFun1,
    fPred1,
    fAndN,
) where

import Data.Map qualified as Map
import Data.Set qualified as Set

import Data.List (intercalate)

import Text.Printf (printf)

-- Dada una fórmula A da su doble negación
dneg :: Form -> Form
dneg f = FNot $ FNot f

-- Dado un id de predicado devuelve un predicado de aridad 0,
-- i.e una variable proposicional (propositional variable)
-- Deprecated: Usar fPred0
propVar :: PredId -> Form
propVar = fPred0

-- Dado un id de predicado devuelve un predicado de aridad 0,
-- i.e una variable proposicional (propositional variable)
fPred0 :: PredId -> Form
fPred0 p = FPred p []

-- Deprecated: Usar fPredVar
predVar :: PredId -> VarId -> Form
predVar = fPredVar

fPredVar :: PredId -> VarId -> Form
fPredVar p v = fPred1 p (TVar v)

fPred1 :: PredId -> Term -> Form
fPred1 p t = FPred p [t]

tFun0 :: FunId -> Term
tFun0 f = TFun f []

tFun1 :: FunId -> Term -> Term
tFun1 f t = TFun f [t]

fAndN :: [Form] -> Form
fAndN = foldl1 FAnd

-- Tipos de identificadores
type VarId = String
type FunId = String
type PredId = String
type HypId = String

-- Sustitución de variables, usado para alphaEqForm y subst sin capturas
type VarSubstitution = Map.Map String String

type Metavar = Int

data Term
    = TVar VarId
    | TMetavar Metavar
    | TFun FunId [Term]

instance Show Term where
    show (TVar x) = x
    show (TFun f ts) = f ++ showArgs ts
    show (TMetavar x) = "?" ++ show x

-- Free variables de un término
fvTerm :: Term -> Set.Set VarId
fvTerm (TVar x) = Set.singleton x
fvTerm (TFun _ ts) = foldr (Set.union . fvTerm) Set.empty ts
fvTerm (TMetavar _) = Set.empty

instance Eq Term where
    (==) = alphaEqTerm Map.empty Map.empty

alphaEqTerm :: VarSubstitution -> VarSubstitution -> Term -> Term -> Bool
alphaEqTerm m1 m2 t u = case (t, u) of
    (TMetavar x, TMetavar y) -> x == y
    (TVar x, TVar y)
        | x == y -> True
        | otherwise -> case Map.lookup x m1 of
            Nothing -> False
            Just x' -> case Map.lookup y m2 of
                Nothing -> False
                Just y' -> x' == y'
    (TFun f1 ts1, TFun f2 ts2) -> f1 == f2 && alphaEqTerms m1 m2 ts1 ts2
    _ -> False -- Diferente forma

alphaEqTerms :: VarSubstitution -> VarSubstitution -> [Term] -> [Term] -> Bool
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

instance Show Form where
    show :: Form -> String
    show (FPred p ts) = p ++ showArgs ts
    show (FAnd l r) = showBinParen l ++ " & " ++ showBinParen r
    show (FOr l r) = showBinParen l ++ " | " ++ showBinParen r
    show (FImp a c)
        | c == fPred0 "__r" = "~r " ++ showBinParen a -- hack for friedman
        | otherwise = showBinParen a ++ " -> " ++ showBinParen c
    show (FNot f) = "~" ++ showBinParen f
    show FTrue = "true"
    show FFalse = "false"
    show (FForall x f) = "forall " ++ x ++ " . " ++ showBinParen f
    show (FExists x f) = "exists " ++ x ++ " . " ++ showBinParen f

-- muestra las binarias con paréntesis, para evitar supérfluos en show
showBinParen :: Form -> String
showBinParen f@(FAnd _ _) = "(" ++ show f ++ ")"
showBinParen f@(FOr _ _) = "(" ++ show f ++ ")"
showBinParen f@(FImp _ _) = "(" ++ show f ++ ")"
showBinParen f = show f

showArgs :: (Show a) => [a] -> String
showArgs [] = ""
showArgs ts = "(" ++ intercalate ", " (map show ts) ++ ")"

instance Eq Form where
    (==) = alphaEqForm' Map.empty Map.empty

alphaEqForm' :: VarSubstitution -> VarSubstitution -> Form -> Form -> Bool
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
-- TODO: Hay que devolver también la próxima libre, sino para un && puede pasar
-- que se use la misma de ambos lados y está mal?
alphaEqForm :: Int -> VarSubstitution -> VarSubstitution -> Form -> Form -> Bool
alphaEqForm n m1 m2 f g = case (f, g) of
    (FTrue, FTrue) -> True
    (FFalse, FFalse) -> True
    (FPred p1 ts1, FPred p2 ts2) -> p1 == p2 && alphaEqTerms m1 m2 ts1 ts2
    (FAnd f1 g1, FAnd f2 g2) -> alphaEqForm n m1 m2 f1 f2 && alphaEqForm n m1 m2 g1 g2
    (FOr f1 g1, FOr f2 g2) -> alphaEqForm n m1 m2 f1 f2 && alphaEqForm n m1 m2 g1 g2
    (FImp f1 g1, FImp f2 g2) -> alphaEqForm n m1 m2 f1 f2 && alphaEqForm n m1 m2 g1 g2
    (FNot f1, FNot f2) -> alphaEqForm n m1 m2 f1 f2
    (FForall x f1, FForall y f2)
        | x == y -> alphaEqForm n m1 m2 f1 f2
        | otherwise ->
            alphaEqForm
                (n + 1)
                (Map.insert x (varN n) m1)
                (Map.insert y (varN n) m2)
                f1
                f2
    (FExists x f1, FExists y f2)
        | x == y -> alphaEqForm n m1 m2 f1 f2
        | otherwise ->
            alphaEqForm
                (n + 1)
                (Map.insert x (varN n) m1)
                (Map.insert y (varN n) m2)
                f1
                f2
    _ -> False -- Diferente forma

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

isForall :: Form -> Bool
isForall (FForall{}) = True
isForall _ = False

data Env
    = EEmpty
    | EExtend HypId Form Env
    deriving (Eq)

instance Show Env where
    show env = "{" ++ intercalate ", " (map showPair envList) ++ "}"
      where
        showPair (h, f) = printf "'%s' : %s" h (show f)
        envList = asList env

fromList :: [(HypId, Form)] -> Env
fromList [] = EEmpty
fromList ((h, f) : ps) = EExtend h f (fromList ps)

asList :: Env -> [(HypId, Form)]
asList EEmpty = []
asList (EExtend h f r) = (h, f) : asList r

get :: Env -> HypId -> Maybe Form
get EEmpty _ = Nothing
get (EExtend hyp' f env) hyp
    | hyp == hyp' = Just f
    | otherwise = get env hyp

forms :: Env -> [Form]
forms EEmpty = []
forms (EExtend _ f e') = f : forms e'

fvE :: Env -> Set.Set VarId
fvE e = foldr (Set.union . fv) Set.empty (forms e)

formsWithFv :: Env -> VarId -> [Form]
formsWithFv env x = filter (elem x . fv) (forms env)

-- Record syntax: https://en.wikibooks.org/wiki/Haskell/More_on_datatypes

data Proof
    = PNamed
        { name :: String
        , proof :: Proof
        }
    | PAx HypId
    | -- A ^ B
      PAndI
        { proofLeft :: Proof -- A
        , proofRight :: Proof -- de B
        }
    | PAndE1
        { right :: Form -- B
        , proofAnd :: Proof -- de A ^ B
        }
    | PAndE2
        { left :: Form -- A
        , proofAnd :: Proof -- de A ^ B
        }
    | -- A v B deduce C
      POrI1
        { proofLeft :: Proof -- A
        }
    | POrI2
        { proofRight :: Proof -- B
        }
    | POrE
        { left :: Form -- A
        , right :: Form -- B
        , proofOr :: Proof -- de A v B
        , hypLeft :: HypId -- x:A
        , proofAssumingLeft :: Proof -- de C asumiendo A
        , hypRight :: HypId -- x:B
        , proofAssumingRight :: Proof -- de C asumiendo B
        }
    | -- A -> B
      PImpI
        { hypAntecedent :: HypId
        , proofConsequent :: Proof
        }
    | PImpE
        { antecedent :: Form -- A
        , proofImp :: Proof -- de A -> B
        , proofAntecedent :: Proof -- de A
        }
    | -- ¬ A = A -> bottom
      -- También conocida como RAA (reducción al absurdo)
      PNotI
        { hyp :: HypId -- x:A
        , proofBot :: Proof -- de bottom
        }
    | PNotE
        { form :: Form -- A
        , proofNotForm :: Proof -- de ~A
        , proofForm :: Proof -- de A
        }
    | PTrueI
    | PFalseE
        { proofBot :: Proof
        }
    | PLEM
    | -- V x . A
      PForallI
        { newVar :: VarId -- x'
        , proofForm :: Proof -- de A { x := x'}
        }
    | PForallE
        { var :: VarId -- x
        , form :: Form -- A (sin sust)
        , proofForall :: Proof -- de V x . A
        , termReplace :: Term -- t
        }
    | -- E x . A
      PExistsI
        { inst :: Term -- t
        , proofFormWithInst :: Proof -- de A {x := t}
        }
    | -- E x . A deduce B
      PExistsE
        { var :: VarId -- x
        , form :: Form -- A
        , proofExists :: Proof -- de E x. A
        , hyp :: HypId -- x: A
        , proofAssuming :: Proof -- de B con A como hyp
        }
    deriving (Show, Eq)

proofName :: Proof -> String
proofName p = case p of
    PAx h -> "PAx " ++ h
    PNamed n _ -> "PNamed " ++ n
    PAndI{} -> "PAndI"
    PAndE1{} -> "PAndE1"
    PAndE2{} -> "PAndE2"
    POrI1{} -> "POrI1"
    POrI2{} -> "POrI2"
    POrE{} -> "POrE"
    PImpI{} -> "PImpI"
    PImpE{} -> "PImpE"
    PNotI{} -> "PNotI"
    PNotE{} -> "PNotE"
    PTrueI{} -> "PTrueI"
    PFalseE{} -> "PFalseE"
    PLEM{} -> "PLEM"
    PForallI{} -> "PForallI"
    PForallE{} -> "PForallE"
    PExistsI{} -> "PExistsI"
    PExistsE{} -> "PExistsE"

fvP :: Proof -> Set.Set VarId
fvP p = case p of
    PAx _ -> Set.empty
    PNamed _ p1 -> fvP p1
    PAndI pL pR -> Set.union (fvP pL) (fvP pR)
    PAndE1 r pR -> Set.union (fv r) (fvP pR)
    PAndE2 l pL -> Set.union (fv l) (fvP pL)
    POrI1 pL -> fvP pL
    POrI2 pR -> fvP pR
    POrE l r pOr _ pL _ pR -> Set.unions [fv l, fv r, fvP pOr, fvP pL, fvP pR]
    PImpI _ p1 -> fvP p1
    PImpE f pI pA -> Set.unions [fv f, fvP pI, fvP pA]
    PNotI _ pB -> fvP pB
    PNotE f pNotF pF -> Set.unions [fv f, fvP pNotF, fvP pF]
    PTrueI -> Set.empty
    PFalseE pB -> fvP pB
    PLEM -> Set.empty
    PForallI _ pF -> fvP pF
    PForallE _ f pF t -> Set.unions [fv f, fvTerm t, fvP pF]
    PExistsI t p1 -> Set.union (fvP p1) (fvTerm t)
    PExistsE _ f pE h pA -> Set.unions [fv f, fvP pE, fvP pA]

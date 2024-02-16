-- El módulo ND (Natural Deduction) contiene tipos para demostraciones de
-- fórmulas de LPO con deducción natural.
module ND (
    Env (..),
    get,
    VarId,
    FunId,
    PredId,
    HypId,
    Term (..),
    Form (..),
    Proof (..),
    Subst,
    fv,
    fvTerm,
    fvE,
    propVar,
    predVar,
    dneg,
) where

import Data.Map qualified as Map
import Data.Set qualified as Set

import Data.List (intercalate)

-- Dada una fórmula A da su doble negación
dneg :: Form -> Form
dneg f = FNot $ FNot f

-- Dado un id de predicado devuelve un predicado de aridad 0,
-- i.e una variable proposicional (propositional variable)
propVar :: PredId -> Form
propVar pid = FPred pid []

predVar :: PredId -> VarId -> Form
predVar p v = FPred p [TVar v]

-- Tipos de identificadores
type VarId = String
type FunId = String
type PredId = String
type HypId = String

-- Sustitución de variables, usado para alphaEqForm y subst sin capturas
type Subst = Map.Map String String

data Term
    = TVar VarId
    | TFun FunId [Term]

instance Show Term where
    show (TVar x) = x
    show (TFun f ts) = f ++ showArgs ts

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

instance Show Form where
    show (FPred p ts) = p ++ showArgs ts
    show (FAnd l r) = showBinParen l ++ " ^ " ++ showBinParen r
    show (FOr l r) = showBinParen l ++ " v " ++ showBinParen r
    show (FImp a c) = showBinParen a ++ " => " ++ showBinParen c
    show (FNot f) = "~" ++ show f
    show FTrue = "true"
    show FFalse = "false"
    show (FForall x f) = "forall x. " ++ showBinParen f
    show (FExists x f) = "exists x. " ++ showBinParen f

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
-- TODO: Hay que devolver también la próxima libre, sino para un && puede pasar
-- que se use la misma de ambos lados y está mal?
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

-- Record syntax: https://en.wikibooks.org/wiki/Haskell/More_on_datatypes

data Proof
    = PNamed String
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

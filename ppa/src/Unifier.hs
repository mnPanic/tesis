module Unifier (
    unifyF,
    unifyT,
    SingleSubst (..),
) where

import ND (
    Form (..),
    Subst,
    Term (..),
    fvTerm,
    varN,
 )

import NDProofs (
    Result (..),
 )

import Data.Map qualified as Map
import Text.Printf (printf)

-- Sustitución de metavariables, usado para unificación (unifyF, unifyT)
data SingleSubst = SSEmpty | SSTerm Term
    deriving (Eq)

instance Show SingleSubst where
    show SSEmpty = "{}"
    show (SSTerm t) = printf "{%s = %s}" (show TMetavar) (show t)

unifyF :: SingleSubst -> Form -> Form -> Result SingleSubst
unifyF = unifyF' 0 Map.empty Map.empty

{- Chequea que dos fórmulas unifiquen bajo alpha equivalencia.
Análogo a la implementación en ND.alphaEqForm
-}
unifyF' :: Int -> Subst -> Subst -> SingleSubst -> Form -> Form -> Result SingleSubst
unifyF' n r1 r2 s f0 g0 = case (f0, g0) of
    (FTrue, FTrue) -> return SSEmpty
    (FFalse, FFalse) -> return SSEmpty
    (FPred p1 ts1, FPred p2 ts2)
        | p1 /= p2 -> Left $ printf "different predicate name (%s /= %s)" p1 p2
        | otherwise -> unifyTs r1 r2 s ts1 ts2
    (FAnd f1 g1, FAnd f2 g2) -> case unifyF' n r1 r2 s f1 f2 of
        Left err -> Left err
        Right s2 -> unifyF' n r1 r2 s2 g1 g2
    (FOr f1 g1, FOr f2 g2) -> case unifyF' n r1 r2 s f1 f2 of
        Left err -> Left err
        Right s2 -> unifyF' n r1 r2 s2 g1 g2
    (FImp f1 g1, FImp f2 g2) -> case unifyF' n r1 r2 s f1 f2 of
        Left err -> Left err
        Right s2 -> unifyF' n r1 r2 s2 g1 g2
    (FNot f1, FNot f2) -> unifyF' n r1 r2 s f1 f2
    (FForall x f1, FForall y f2)
        | x == y -> unifyF' n r1 r2 s f1 f2
        | otherwise ->
            let freshVar = varN n
                r1' = Map.insert x freshVar r1
                r2' = Map.insert y freshVar r2
             in unifyF' (n + 1) r1' r2' s f1 f2
    (FExists x f1, FExists y f2)
        | x == y -> unifyF' n r1 r2 s f1 f2
        | otherwise ->
            let freshVar = varN n
                r1' = Map.insert x freshVar r1
                r2' = Map.insert y freshVar r2
             in unifyF' (n + 1) r1' r2' s f1 f2
    (f0, g0) -> Left $ printf "different form types: %s /= %s" (show f0) (show g0)

unifyT :: Subst -> Subst -> SingleSubst -> Term -> Term -> Result SingleSubst
unifyT r1 r2 subst t0 s0 =
    let t = representative subst t0
        s = representative subst s0
     in case (t, s) of
            (TMetavar, TMetavar) -> Right subst
            -- Para los casos de un término con una metavar, no hace falta
            -- "compatibilizar" las substs, porque ya se hace implícitamente
            -- al buscar los representantes. Si la metavar tuviera otro termino
            -- que t, acá estaría comparando contra eso y no una metavar.
            (TMetavar, t)
                | occurs t -> Left $ printf "Occurs check: %s has metavars, can't {?\\%s}" (show t) (show t)
                | occursInRename r2 t -> Left $ printf "Rename check: '%s' has free variables that were renamed for alpha equivalence" (show t)
                | otherwise -> Right $ SSTerm t
            (t, TMetavar)
                | occurs t -> Left $ printf "Occurs check: %s has metavars, can't {?\\%s}" (show t) (show t)
                | occursInRename r1 t -> Left $ printf "Rename check: '%s' has free variables that were renamed for alpha equivalence" (show t)
                | otherwise -> Right $ SSTerm t
            (TVar x1, TVar x2)
                | x1 == x2 -> Right subst
                | otherwise -> case Map.lookup x1 r1 of
                    Nothing -> Left $ printf "different var names: %s /= %s" x1 x2
                    Just x1' -> case Map.lookup x2 r2 of
                        Nothing -> Left $ printf "different var names: %s (%s) /= %s" x1 x1' x2
                        Just x2'
                            | x1' == x2' -> Right subst
                            | otherwise -> Left $ printf "different var names: %s (%s) /= %s (%s)" x1 x1' x2 x2'
            (TFun f1 ts1, TFun f2 ts2)
                | f1 /= f2 -> Left $ printf "different function names: %s /= %s" f1 f2
                | otherwise -> unifyTs r1 r2 subst ts1 ts2
            (t, s) -> Left $ printf "different term types: %s /= %s" (show t) (show s)

-- Chequea que el termino no contenga metavariables (occurs check)
-- Asume que t es un representante
occurs :: Term -> Bool
occurs t = case t of
    TMetavar -> True
    TVar _ -> False
    TFun _ ts -> any occurs ts

{- Chequea que el término al cual instanciar una metavar no tenga variables
libres que aparezcan en su dominio de renombre.
Esto sirve para evitar instanciar metavars en variables ligadas fuera del scope
del cuantificador, por ej.

forall y. (forall x. ?) & x =.=
forall x. (forall x. x) & ?

si se unificara ?=x, entonces se ligaría erróneamente en la 2da fórmula
-}
occursInRename :: Subst -> Term -> Bool
occursInRename r t = any (\x -> x `elem` Map.keys r) (fvTerm t)

unifyTs :: Subst -> Subst -> SingleSubst -> [Term] -> [Term] -> Result SingleSubst
unifyTs r1 r2 subst [] [] = Right subst
unifyTs r1 r2 subst _ [] = Left "different length"
unifyTs r1 r2 subst [] _ = Left "different length"
unifyTs r1 r2 subst (t : ts) (s : ss) = case unifyT r1 r2 subst t s of
    Left err -> Left err
    Right subst' -> unifyTs r1 r2 subst' ts ss

representative :: SingleSubst -> Term -> Term
representative s TMetavar = case s of
    SSEmpty -> TMetavar
    SSTerm t -> t
representative _ t = t

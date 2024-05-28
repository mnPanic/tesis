module Unifier (
    unifyF,
    unifyT,
    SingleSubst (..),
) where

import ND (
    Form (..),
    Term (..),
 )

import NDProofs (
    Result (..),
 )

import Text.Printf (printf)

data SingleSubst = SSEmpty | SSTerm Term
    deriving (Show, Eq)

-- TODO: Considerar alpha igualdad para unificación.
unifyF :: SingleSubst -> Form -> Form -> Result SingleSubst
unifyF s f0 g0 = case (f0, g0) of
    (FTrue, FTrue) -> return SSEmpty
    (FFalse, FFalse) -> return SSEmpty
    (FPred p1 ts1, FPred p2 ts2)
        | p1 /= p2 -> Left $ printf "different predicate name (%s /= %s)" p1 p2
        | otherwise -> unifyTs s ts1 ts2
    (FAnd f1 g1, FAnd f2 g2) -> case unifyF s f1 f2 of
        Left err -> Left err
        Right s2 -> unifyF s2 g1 g2
    (FOr f1 g1, FOr f2 g2) -> case unifyF s f1 f2 of
        Left err -> Left err
        Right s2 -> unifyF s2 g1 g2
    (FImp f1 g1, FImp f2 g2) -> case unifyF s f1 f2 of
        Left err -> Left err
        Right s2 -> unifyF s2 g1 g2
    (FNot f1, FNot f2) -> unifyF s f1 f2
    (FForall x f1, FForall y f2)
        | x == y -> unifyF s f1 f2
        | otherwise -> Left $ printf "different vars (%s /= %s)" x y
    (FExists x f1, FExists y f2)
        | x == y -> unifyF s f1 f2
        | otherwise -> Left $ printf "different vars (%s /= %s)" x y
    (f0, g0) -> Left $ printf "different form types: %s /= %s" (show f0) (show g0)

-- TODO: resolver bien
unifyT :: SingleSubst -> Term -> Term -> Result SingleSubst
unifyT subst t0 s0 =
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
                | otherwise -> Right $ SSTerm t
            (t, TMetavar)
                | occurs t -> Left $ printf "Occurs check: %s has metavars, can't {?\\%s}" (show t) (show t)
                | otherwise -> Right $ SSTerm t
            (TVar x1, TVar x2)
                | x1 == x2 -> Right subst
                | otherwise -> Left $ printf "different var names: %s /= %s" x1 x2
            (TFun f1 ts1, TFun f2 ts2)
                | f1 /= f2 -> Left $ printf "different function names: %s /= %s" f1 f2
                | otherwise -> unifyTs subst ts1 ts2
            (t, s) -> Left $ printf "different term types: %s /= %s" (show t) (show s)

-- Chequea que el termino no contenga metavariables (occurs check)
-- Asume que t es un representante
occurs :: Term -> Bool
occurs t = case t of
    TMetavar -> True
    TVar _ -> False
    TFun _ ts -> any occurs ts

unifyTs :: SingleSubst -> [Term] -> [Term] -> Result SingleSubst
unifyTs subst [] [] = Right subst
unifyTs subst _ [] = Left "different length"
unifyTs subst [] _ = Left "different length"
unifyTs subst (t : ts) (s : ss) = case unifyT subst t s of
    Left err -> Left err
    Right subst' -> unifyTs subst' ts ss

representative :: SingleSubst -> Term -> Term
representative s TMetavar = case s of
    SSEmpty -> TMetavar
    SSTerm t -> t
representative _ t = t

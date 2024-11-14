module PPA.Unifier (
    unifyF,
    unifyT,
    Substitution,
    showSubstitution,
) where

import ND.ND (
    Form (..),
    Metavar,
    Term (..),
    VarSubstitution,
    fvTerm,
    varN,
 )

import Result (
    Result,
 )

import Data.List (intercalate)
import Data.Map qualified as Map
import Text.Printf (printf)

-- Sustitución de metavariables, usado para unificación (unifyF, unifyT)
type Substitution = Map.Map Metavar Term

showSubstitution :: Substitution -> String
showSubstitution s =
    let pairs = Map.foldrWithKey (\k v r -> (show k ++ "=" ++ show v) : r) [] s
     in "{" ++ intercalate ", " pairs ++ "}"

unifyF :: Substitution -> Form -> Form -> Result Substitution
unifyF = unifyF' 0 Map.empty Map.empty

{- Chequea que dos fórmulas unifiquen bajo alpha equivalencia.
Análogo a la implementación en ND.alphaEqForm
-}
unifyF' :: Int -> VarSubstitution -> VarSubstitution -> Substitution -> Form -> Form -> Result Substitution
unifyF' n r1 r2 s f0 g0 = case (f0, g0) of
    (FTrue, FTrue) -> return s
    (FFalse, FFalse) -> return s
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
    _ -> Left $ printf "different form types: %s /= %s" (show f0) (show g0)

unifyT :: VarSubstitution -> VarSubstitution -> Substitution -> Term -> Term -> Result Substitution
unifyT r1 r2 subst t0 s0 =
    let t = representative subst t0
        s = representative subst s0
     in case (t, s) of
            (TMetavar x, TMetavar y) ->
                if x == y
                    then Right subst
                    else Left $ printf "different metavars: %s /= %s" (show x) (show y)
            -- Para los casos de un término con una metavar, no hace falta
            -- "compatibilizar" las substs, porque ya se hace implícitamente
            -- al buscar los representantes. Si la metavar tuviera otro termino
            -- que t, acá estaría comparando contra eso y no una metavar.
            (TMetavar x, t')
                | occurs subst x t' -> Left $ printf "Occurs check: %s occurs in %s, can't {?%s\\%s}" (show x) (show t') (show x) (show t')
                | occursInRename r2 t' -> Left $ printf "Rename check: '%s' has free variables that were renamed for alpha equivalence" (show t')
                | otherwise -> Right $ Map.insert x t' subst
            (t', TMetavar x)
                | occurs subst x t' -> Left $ printf "Occurs check: %s occurs in %s, can't {?%s\\%s}" (show x) (show t') (show x) (show t')
                | occursInRename r1 t' -> Left $ printf "Rename check: '%s' has free variables that were renamed for alpha equivalence" (show t')
                | otherwise -> Right $ Map.insert x t' subst
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
            (_, _) -> Left $ printf "different term types: %s /= %s" (show t) (show s)

-- Chequea que en el término no ocurra una metavariable (occurs check)
-- Asume que t es un representante
occurs :: Substitution -> Metavar -> Term -> Bool
occurs subst x t0 =
    let t = representative subst t0
     in case t of
            TMetavar y -> x == y
            TVar _ -> False
            TFun _ ts -> any (occurs subst x) ts

{- Chequea que el término al cual instanciar una metavar no tenga variables
libres que aparezcan en su dominio de renombre.
Esto sirve para evitar instanciar metavars en variables ligadas fuera del scope
del cuantificador, por ej.

forall y. (forall x. ?1) & x =.=
forall x. (forall x. x) & ?1

si se unificara ?=x, entonces se ligaría erróneamente en la 2da fórmula
-}
occursInRename :: VarSubstitution -> Term -> Bool
occursInRename r t = any (\x -> x `elem` Map.keys r) (fvTerm t)

unifyTs :: VarSubstitution -> VarSubstitution -> Substitution -> [Term] -> [Term] -> Result Substitution
unifyTs _ _ subst [] [] = Right subst
unifyTs _ _ _ _ [] = Left "different length"
unifyTs _ _ _ [] _ = Left "different length"
unifyTs r1 r2 subst (t : ts) (s : ss) = case unifyT r1 r2 subst t s of
    Left err -> Left err
    Right subst' -> unifyTs r1 r2 subst' ts ss

{- Recorre todo el camino de sustituciones hasta llegar al final, obteniendo
un representante de un término.

Por ej puede haber

    { TMetavar 1 = TMetavar 2, TMetavar 2 = TVar "x"}

el repr de (TMetavar 1) es (TVar "x")
-}
representative :: Substitution -> Term -> Term
representative subst m@(TMetavar x) = case Map.lookup x subst of
    Nothing -> m
    Just t -> representative subst t
representative _ t = t

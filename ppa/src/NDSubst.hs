-- Implementa diferentes sustituciones para ND, tanto fórmulas como
-- demostraciones.
module NDSubst (
  subst,
  substHyp,
  substVar,
  freshWRT,
) where

import ND (
  Form (..),
  HypId,
  Proof (..),
  Term (..),
  VarId,
  VarSubstitution,
  fv,
  fvP,
  fvTerm,
 )

import Control.DeepSeq (force)
import Data.Map qualified as Map
import Data.Set (notMember)
import Data.Set qualified as Set

{------------------------ Sustituciones sobre fórmulas ------------------------}

-- sustituye todas las ocurrencias libres de VarId por Term.
--
-- Lo hace alpha-renombrando en el camino para evitar *capturas* de variables:
-- si y esta libre en T, busca y' que no este libre en T y f1. En lugar de
-- reemplazar en el momento, lo guarda en una VarSubstitution para hacerlo lineal y no
-- cuadrático.
subst :: VarId -> Term -> Form -> Form
subst = subst' Map.empty

-- Sustituye ocurrencias libres de x por t en f
-- TODO: Probablemente se puede unir la sustitución original en VarSubstitution
subst' :: VarSubstitution -> VarId -> Term -> Form -> Form
subst' s x t f = case f of
  FPred l ts -> FPred l (map (substTerm s x t) ts)
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
    | y `elem` fvTerm t -> FForall y' (recRenaming y y' f1)
    | otherwise -> FForall y (rec f1)
   where
    y' = freshWRT y $ fv f1 `Set.union` fvTerm t
  orig@(FExists y f1)
    -- No está libre, no cambio
    | x == y -> orig
    -- Captura de variables
    | y `elem` fvTerm t -> FExists y' (recRenaming y y' f1)
    | otherwise -> FExists y (rec f1)
   where
    y' = freshWRT y $ fv f1 `Set.union` fvTerm t
 where
  rec = subst' s x t
  recRenaming y y' = subst' (Map.insert y y' s) x t

-- Sustituye x por t en t'
substTerm :: VarSubstitution -> VarId -> Term -> Term -> Term
substTerm s x t t' = case t' of
  -- No es necesario chequear que el renombre de x se quiera sustituir, porque
  -- renombramos cuando encontramos un cuantificador, y si la var del
  -- cuantificador es la misma de la subst entonces no está libre y hubiera
  -- cortado (nunca llega acá)
  o@(TVar y)
    | x == y -> t
    | otherwise -> maybe o TVar (Map.lookup y s)
  TFun f ts -> TFun f (map (substTerm s x t) ts)
  m@(TMetavar _) -> m

-- freshWRT da una variable libre con respecto a una lista en donde no queremos
-- que aparezca
freshWRT :: VarId -> Set.Set VarId -> VarId
freshWRT x forbidden = head [x ++ suffix | suffix <- map show [0 :: Integer ..], (x ++ suffix) `notMember` forbidden]

{--------------------- Sustituciones sobre demostraciones ---------------------}

-- Sustitución de hipótesis, usado para substHyp sin capturas
type HypSubstitution = Map.Map HypId HypId

substVar :: VarId -> Term -> Proof -> Proof
substVar = substVar' Map.empty

-- Hace (subst varid term) en toda la proof recursivamente
substVar' :: VarSubstitution -> VarId -> Term -> Proof -> Proof
substVar' s x t proof = case proof of
  PNamed name p1 -> PNamed name (rec p1)
  PAx h -> PAx h
  PAndI pL pR -> PAndI (rec pL) (rec pR)
  PAndE1 r pR -> PAndE1 (doSubst r) (rec pR)
  PAndE2 l pL -> PAndE2 (doSubst l) (rec pL)
  POrI1 pL -> POrI1 (rec pL)
  POrI2 pR -> POrI2 (rec pR)
  POrE l r pOr hL pL hR pR -> POrE (doSubst l) (doSubst r) (rec pOr) hL (rec pL) hR (rec pR)
  PImpI h p1 -> PImpI h (rec p1)
  PImpE f pI pA -> PImpE (doSubst f) (rec pI) (rec pA)
  PNotI h pB -> PNotI h (rec pB)
  PNotE f pNotF pF -> PNotE (doSubst f) (rec pNotF) (rec pF)
  PTrueI -> PTrueI
  PFalseE pB -> PFalseE (rec pB)
  PLEM -> PLEM
  p@(PForallI y pF)
    | x == y -> p
    | y `elem` fvTerm t ->
        let y' = freshWRT y (Set.union (fvTerm t) (fvP pF))
            s' = Map.insert y y' s
         in PForallI y' (recS s' pF)
    | otherwise -> PForallI y (rec pF)
  PForallE y f pF t'
    -- Cortas cambiando T (todavía no está en el scope del nuevo x, pero f si)
    -- También seguís por pF, porque no está alcanzado por el scope de y.
    | x == y -> PForallE y f (rec pF) (doSubstT t')
    -- Seguis chequeando que no haya captura
    | y `elem` fvTerm t ->
        let y' = freshWRT y (Set.unions [fvTerm t, fvP pF, fv f])
            s' = Map.insert y y' s
         in PForallE y' (doSubstS s' f) (recS s' pF) (doSubstT t')
    | otherwise -> PForallE y (doSubst f) (rec pF) (doSubstT t')
  PExistsI t' p1 -> PExistsI (doSubstT t') (rec p1)
  -- TODO tests estos casos
  PExistsE y f pE h pA
    | x == y -> PExistsE y f (rec pE) h pA
    | y `elem` fvTerm t ->
        let y' = freshWRT y (Set.unions [fvTerm t, fvP pA, fvP pE, fv f])
            s' = Map.insert y y' s
         in PExistsE y' (doSubstS s' f) (recS s' pE) h (recS s' pA)
    | otherwise -> PExistsE y (doSubst f) (rec pE) h (rec pA)
 where
  doSubst = subst' s x t
  doSubstS s' = subst' s' x t
  doSubstT = substTerm s x t
  rec = substVar' s x t
  recS s' = substVar' s' x t

{- Sustituye todos los usos de una hipótesis por una demostración
Sin capturas.

Una captura sería que reemplazando en una demo que agregue la hip h', y que
en p' se use esa hyp. Hay que renombrar la que se agrega por otra que no se
use en p' ni en la demo subsiguiente.
-}
substHyp :: HypId -> Proof -> Proof -> Proof
substHyp h p' = substHyp' Map.empty h p' hypsP
 where
  hypsP = citedHypIds p'

substHyp' :: HypSubstitution -> HypId -> Proof -> Set.Set HypId -> Proof -> Proof
substHyp' s hRep pRep hypsPRep p = case p of
  PNamed name p1 -> PNamed name (rec p1)
  PAx h'
    | h' == hRep -> pRep
    | otherwise -> case Map.lookup h' s of
        -- No puede pasar que esté renombrado y el renombre coincida con lo
        -- que se quiere renombrar, si hubiera sido la misma, hubiera cortado
        Just h'' -> PAx h''
        Nothing -> PAx h'
  PAndI pL pR -> PAndI (rec pL) (rec pR)
  PAndE1 r pR -> PAndE1 r (rec pR)
  PAndE2 l pL -> PAndE2 l (rec pL)
  POrI1 pL -> POrI1 (rec pL)
  POrI2 pR -> POrI2 (rec pR)
  POrE l r pOr hL pL hR pR -> POrE l r (rec pOr) hL' pL' hR' pR'
   where
    (hL', pL') = recAvoidingCapture hL pL
    (hR', pR') = recAvoidingCapture hR pR
  PImpI h p1 -> PImpI h' p1'
   where
    (h', p1') = recAvoidingCapture h p1
  PImpE f pI pA -> PImpE f (rec pI) (rec pA)
  PNotI h pB -> PNotI h' pB'
   where
    (h', pB') = recAvoidingCapture h pB
  PNotE f pNotF pF -> PNotE f (rec pNotF) (rec pF)
  PTrueI -> PTrueI
  PFalseE pB -> PFalseE (rec pB)
  PLEM -> PLEM
  PForallI x pF -> PForallI x (rec pF)
  PForallE x f pF t -> PForallE x f (rec pF) t
  PExistsI t p1 -> PExistsI t (rec p1)
  PExistsE x f pE h pA -> PExistsE x f (rec pE) h' pA'
   where
    (h', pA') = recAvoidingCapture h pA
 where
  rec = substHyp' s hRep pRep hypsPRep
  recAvoidingCapture = substHypAvoidCapture s hRep pRep hypsPRep

-- Reemplazando hReplace por pReplace, nos encontramos con una sub dem que tiene
-- h como hipotesis y p como sub-demo. Queremos reemplazar en p sin que eso
-- genere una captura (si pReplace usa h, hay que renombrar h por h' en p)
substHypAvoidCapture ::
  HypSubstitution ->
  HypId ->
  Proof ->
  Set.Set HypId ->
  HypId ->
  Proof ->
  (HypId, Proof)
substHypAvoidCapture s hReplace pReplace hypsPReplace h p
  -- Cortamos porque se re-definió la hyp que queremos reemplazar, resetea scope.
  | hReplace == h = (h, p)
  -- Hay captura
  | h `elem` hypsPReplace =
      let pHyps = citedHypIds p
          h' = force freshWRT h (Set.union hypsPReplace pHyps)
          p' = substHyp' (Map.insert h h' s) hReplace pReplace hypsPReplace p
       in (h', p')
  -- No hay captura, sigue normalmente
  | otherwise =
      let p' = substHyp' s hReplace pReplace hypsPReplace p
       in (h, p')

citedHypIds :: Proof -> Set.Set HypId
citedHypIds p = case p of
  PAx h -> Set.singleton h
  PNamed _ p1 -> citedHypIds p1
  PAndI pL pR -> Set.union (citedHypIds pL) (citedHypIds pR)
  PAndE1 _ pR -> citedHypIds pR
  PAndE2 _ pL -> citedHypIds pL
  POrI1 pL -> citedHypIds pL
  POrI2 pR -> citedHypIds pR
  POrE _ _ pOr _ pL _ pR -> Set.unions [citedHypIds pOr, citedHypIds pL, citedHypIds pR]
  PImpI _ p1 -> citedHypIds p1
  PImpE _ pI pA -> Set.union (citedHypIds pI) (citedHypIds pA)
  PNotI _ pB -> citedHypIds pB
  PNotE _ pNotF pF -> Set.union (citedHypIds pNotF) (citedHypIds pF)
  PTrueI -> Set.empty
  PFalseE pB -> citedHypIds pB
  PLEM -> Set.empty
  PForallI _ pF -> citedHypIds pF
  PForallE{proofForall = pF} -> citedHypIds pF
  PExistsI _ p1 -> citedHypIds p1
  PExistsE{proofExists = pE, proofAssuming = pA} -> Set.union (citedHypIds pE) (citedHypIds pA)
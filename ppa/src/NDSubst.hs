-- Implementa diferentes sustituciones para ND, tanto fórmulas como
-- demostraciones.
module NDSubst (
  subst,
  substHyp,
  substVar,
  HypMemo,
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
  proofName,
 )

import Data.Map qualified as Map
import Data.Set (notMember)
import Data.Set qualified as Set
import Debug.Trace (trace)
import Text.Printf (printf)

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
freshWRT x forbidden = head [x ++ suffix | suffix <- map show [0 ..], (x ++ suffix) `notMember` forbidden]

{--------------------- Sustituciones sobre demostraciones ---------------------}

-- TODO: Se puede refactorizar para que use state monad y evitar repetición
type HypMemo = Map.Map Proof (Set.Set HypId)

-- Sustitución de hipótesis, usado para substHyp sin capturas
type HypSubstitution = Map.Map HypId HypId

substVar :: VarId -> Term -> Proof -> Proof
substVar = substVar' Map.empty

-- Hace (subst varid term) en toda la proof recursivamente
substVar' :: VarSubstitution -> VarId -> Term -> Proof -> Proof
substVar' s x t p = case p of
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
  p@(PForallE y f pF t')
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
  p@(PExistsE y f pE h pA)
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
substHyp :: HypMemo -> Int -> HypId -> Proof -> Proof -> (HypMemo, Proof)
substHyp mem idt h p' =
  let (mem', hypsP) = citedHypIds mem p'
   in substHyp' mem' Map.empty idt h p' hypsP

indent :: Int -> String
indent n = concat $ replicate (n * 2) "  "

substHyp' :: HypMemo -> HypSubstitution -> Int -> HypId -> Proof -> Set.Set HypId -> Proof -> (HypMemo, Proof)
-- substHyp' s h p' p | trace (printf "substHyp'") False = undefined
-- substHyp' s idt hRep pRep hypsPRep p | trace (printf "%ssubstHyp' %s %s %s" (indent idt) hRep (proofName pRep) (proofName p)) False = undefined
substHyp' mem s idt hRep pRep hypsPRep p = case p of
  PNamed name p1 -> (mem', PNamed name p1')
   where
    (mem', p1') = rec mem p1
  PAx h'
    | h' == hRep -> (mem, pRep)
    | otherwise -> case Map.lookup h' s of
        -- No puede pasar que esté renombrado y el renombre coincida con lo
        -- que se quiere renombrar, si hubiera sido la misma, hubiera cortado
        Just h'' -> (mem, PAx h'')
        Nothing -> (mem, PAx h')
  PAndI pL pR ->
    let (mem1, pL') = rec mem pL
        (mem2, pR') = rec mem1 pR
     in (mem2, PAndI pL' pR')
  PAndE1 r pR -> (mem', PAndE1 r pR')
   where
    (mem', pR') = rec mem pR
  PAndE2 l pL -> (mem', PAndE2 l pL')
   where
    (mem', pL') = rec mem pL
  POrI1 pL -> let in (mem', POrI1 pL')
   where
    (mem', pL') = rec mem pL
  POrI2 pR -> (mem', POrI2 pR')
   where
    (mem', pR') = rec mem pR
  POrE l r pOr hL pL hR pR ->
    let
      (mem1, pOr') = rec mem pOr
      (mem2, hL', pL') = recAvoidingCapture mem1 hL pL
      (mem3, hR', pR') = recAvoidingCapture mem2 hL pR
     in
      (mem3, POrE l r pOr' hL' pL' hR' pR')
  PImpI h p1 -> (mem', PImpI h' p1')
   where
    (mem', h', p1') = recAvoidingCapture mem h p1
  PImpE f pI pA -> (mem2, PImpE f pI' pA')
   where
    (mem1, pI') = rec mem pI
    (mem2, pA') = rec mem1 pA
  PNotI h pB -> (mem', PNotI h' pB')
   where
    (mem', h', pB') = recAvoidingCapture mem h pB
  PNotE f pNotF pF -> (mem2, PNotE f pNotF' pF')
   where
    (mem1, pNotF') = rec mem pNotF
    (mem2, pF') = rec mem1 pF
  PTrueI -> (mem, PTrueI)
  PFalseE pB -> (mem', PFalseE pB')
   where
    (mem', pB') = rec mem pB
  PLEM -> (mem, PLEM)
  PForallI x pF -> (mem', PForallI x pF')
   where
    (mem', pF') = rec mem pF
  PForallE x f pF t -> (mem', PForallE x f pF' t)
   where
    (mem', pF') = rec mem pF
  PExistsI t p1 -> (mem', PExistsI t p1')
   where
    (mem', p1') = rec mem p1
  PExistsE x f pE h pA -> (mem2, PExistsE x f pE' h' pA')
   where
    (mem1, pE') = rec mem pE
    (mem2, h', pA') = recAvoidingCapture mem1 h pA
 where
  rec mem = substHyp' mem s (idt + 1) hRep pRep hypsPRep
  recAvoidingCapture mem = substHypAvoidCapture mem s (idt + 1) hRep pRep hypsPRep

-- Reemplazando hReplace por pReplace, nos encontramos con una sub dem que tiene
-- h como hipotesis y p como sub-demo. Queremos reemplazar en p sin que eso
-- genere una captura (si pReplace usa h, hay que renombrar h por h' en p)
substHypAvoidCapture ::
  HypMemo ->
  HypSubstitution ->
  Int ->
  HypId ->
  Proof ->
  Set.Set HypId ->
  HypId ->
  Proof ->
  (HypMemo, HypId, Proof)
-- substHypAvoidCapture s h p' h' p | trace "substHypAvoidCapture" False = undefined
substHypAvoidCapture mem s idt hReplace pReplace hypsPReplace h p
  -- Cortamos porque se re-definió la hyp que queremos reemplazar, resetea scope.
  | hReplace == h = (mem, h, p)
  -- Hay captura
  | h `elem` hypsPReplace =
      let (mem1, pHyps) = citedHypIds mem p
          h' = freshWRT h (Set.union hypsPReplace pHyps)
          (mem2, p') = substHyp' mem1 (Map.insert h h' s) idt hReplace pReplace hypsPReplace p
       in (mem2, h', p')
  -- No hay captura, sigue normalmente
  | otherwise =
      let (mem1, hyps) = substHyp' mem s idt hReplace pReplace hypsPReplace p
       in (mem1, h, hyps)

citedHypIds :: HypMemo -> Proof -> (HypMemo, Set.Set HypId)
citedHypIds mem p = case Map.lookup p mem of
  Just hyps -> (mem, hyps)
  Nothing ->
    let (mem', hyps) = citedHypIds' mem p
     in (Map.insert p hyps mem', hyps)

citedHypIds' :: HypMemo -> Proof -> (HypMemo, Set.Set HypId)
citedHypIds' mem p = case p of
  PAx h -> (mem, Set.singleton h)
  PNamed name p1 -> rec p1
  PAndI pL pR ->
    let (mem1, hyps1) = rec pL
        (mem2, hyps2) = citedHypIds mem1 pR
     in (mem2, Set.union hyps1 hyps2)
  PAndE1 r pR -> rec pR
  PAndE2 l pL -> rec pL
  POrI1 pL -> rec pL
  POrI2 pR -> rec pR
  POrE l r pOr hL pL hR pR ->
    let (mem1, hyps1) = citedHypIds mem pOr
        (mem2, hyps2) = citedHypIds mem1 pL
        (mem3, hyps3) = citedHypIds mem2 pR
     in (mem3, Set.unions [hyps1, hyps2, hyps3])
  PImpI h p1 -> rec p1
  PImpE f pI pA ->
    let (mem1, hyps1) = citedHypIds mem pI
        (mem2, hyps2) = citedHypIds mem1 pA
     in (mem2, Set.union hyps1 hyps2)
  PNotI h pB -> rec pB
  PNotE f pNotF pF ->
    let (mem1, hyps1) = citedHypIds mem pNotF
        (mem2, hyps2) = citedHypIds mem1 pF
     in (mem2, Set.union hyps1 hyps2)
  PTrueI -> (mem, Set.empty)
  PFalseE pB -> rec pB
  PLEM -> (mem, Set.empty)
  PForallI x pF -> rec pF
  PForallE x f pF t -> rec pF
  PExistsI t p1 -> rec p1
  PExistsE x f pE h pA ->
    let (mem1, hyps1) = citedHypIds mem pE
        (mem2, hyps2) = citedHypIds mem1 pA
     in (mem2, Set.union hyps1 hyps2)
 where
  rec = citedHypIds mem

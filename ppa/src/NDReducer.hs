-- NDReducer contiene lo necesario para poder reducir una ND.Proof
-- a una equivalente más chica.
module NDReducer (reduce, substHyp) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import ND (HypId, Proof (..), Term (TVar), VarId, VarSubstitution, fvTerm)
import NDChecker (subst, subst', substTerm)

-- Sustituciones sobre demostraciones

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
  -- TODO: tests para todos estos casos, solo probé PForallE con misma var
  p@(PForallI y pF)
    | x == y -> p
    -- TODO: acá hay que renombrar y si está libre en t seguro
    | otherwise -> PForallI y (rec pF)
  p@(PForallE y f pF t')
    -- cortas cambiando T (todavía no está en el scope del nuevo x)
    | x == y -> PForallE y f pF (doSubstT t')
    -- Seguis chequeando que no haya captura
    | y `elem` fvTerm t ->
        let s' = Map.insert y y' s
         in PForallE y' (doSubstS s' f) (recS s' pF) (doSubstT t')
    | otherwise -> PForallE y (doSubst f) (rec pF) (doSubstT t')
   where
    y' = freshWRT y (fvTerm t) -- TODO más?
  PExistsI t' p1 -> PExistsI (doSubstT t') (rec p1)
  p@(PExistsE y f pE h pA)
    | x == y -> p
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
substHyp = substHyp' Map.empty

substHyp' :: HypSubstitution -> HypId -> Proof -> Proof -> Proof
substHyp' s h p' p = case p of
  PNamed name p1 -> PNamed name (rec p1)
  PAx h'
    | h' == h -> p' -- mal, captura
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
  rec = substHyp' s h p'
  recAvoidingCapture = substHypAvoidCapture s h p'
  hypsP' = citedHypIds p'

-- Reemplazando hReplace por pReplace, nos encontramos con una sub dem que tiene
-- h como hipotesis y p como sub-demo. Queremos reemplazar en p sin que eso
-- genere una captura (si pReplace usa h, hay que renombrar h por h' en p)
substHypAvoidCapture :: HypSubstitution -> HypId -> Proof -> HypId -> Proof -> (HypId, Proof)
substHypAvoidCapture s hReplace pReplace h p
  -- Cortamos porque se re-definió la hyp que queremos reemplazar
  | hReplace == h = (h, p)
  -- Hay captura
  | h `elem` hypsPReplace =
      let h' = freshWRT h (Set.union hypsPReplace (citedHypIds p))
          p' = substHyp' (Map.insert h h' s) hReplace pReplace p
       in (h', p')
  -- No hay captura, sigue normalmente
  | otherwise = (h, substHyp' s hReplace pReplace p)
 where
  hypsPReplace = citedHypIds pReplace

citedHypIds :: Proof -> Set.Set HypId
citedHypIds p = case p of
  PAx h -> Set.singleton h
  PNamed name p1 -> citedHypIds p1
  PAndI pL pR -> Set.union (citedHypIds pL) (citedHypIds pR)
  PAndE1 r pR -> citedHypIds pR
  PAndE2 l pL -> citedHypIds pL
  POrI1 pL -> citedHypIds pL
  POrI2 pR -> citedHypIds pR
  POrE l r pOr hL pL hR pR -> Set.unions [citedHypIds pOr, citedHypIds pL, citedHypIds pR]
  PImpI h p1 -> citedHypIds p1
  PImpE f pI pA -> Set.union (citedHypIds pI) (citedHypIds pA)
  PNotI h pB -> citedHypIds pB
  PNotE f pNotF pF -> Set.union (citedHypIds pNotF) (citedHypIds pF)
  PTrueI -> Set.empty
  PFalseE pB -> citedHypIds pB
  PLEM -> Set.empty
  PForallI x pF -> citedHypIds pF
  PForallE x f pF t -> citedHypIds pF
  PExistsI t p1 -> citedHypIds p1
  PExistsE x f pE h pA -> Set.union (citedHypIds pE) (citedHypIds pA)

-- freshWRT da una hyp no usada con respecto a una lista en donde no queremos
-- que aparezca
freshWRT :: (Foldable t) => HypId -> t HypId -> HypId
freshWRT h forbidden = head [h ++ suffix | suffix <- map show [0 ..], h ++ suffix `notElem` forbidden]

reduce :: Proof -> Proof
reduce p = maybe p reduce (reduce1 p)

-- Realiza un paso small step de reducción de la demostración
-- Devuelve Nothing si es irreducible.
reduce1 :: Proof -> Maybe Proof
reduce1 p = case p of
  -- Reducción de And
  -- PAndEi(PAndI(Pi_1, Pi_2)) -> PI_i
  PAndE1 right (PAndI proofLeft proofRight) -> Just proofLeft
  PAndE2 left (PAndI proofLeft proofRight) -> Just proofRight
  -- Reducción de Or
  -- POrE(POrI(Pi), h.proofLeft)
  POrE
    { proofOr = POrI1{proofLeft = proofLeft}
    , hypLeft = hypLeft
    , proofAssumingLeft = proofAssumingLeft
    } -> Just $ substHyp hypLeft proofLeft proofAssumingLeft
  POrE
    { proofOr = POrI2{proofRight = proofRight}
    , hypRight = hypRight
    , proofAssumingRight = proofAssumingRight
    } -> Just $ substHyp hypRight proofRight proofAssumingRight
  -- Reducción de Imp
  PImpE
    { antecedent = ant
    , proofImp =
      PImpI
        { hypAntecedent = hypAntecedent
        , proofConsequent = proofAntThenCons
        }
    , proofAntecedent = proofAnt
    } -> Just $ substHyp hypAntecedent proofAnt proofAntThenCons
  -- Reducción de Not
  PNotE
    { form = form
    , proofNotForm =
      PNotI
        { hyp = hypForm
        , proofBot = proofBot
        }
    , proofForm = proofForm
    } -> Just $ substHyp hypForm proofForm proofBot
  -- Reducción de Forall
  PForallE
    { var = x
    , form = form
    , proofForall =
      PForallI
        { newVar = x'
        , proofForm = proofForm
        }
    , termReplace = t
    } -> Just $ substVar x' t proofForm
  -- Valores
  PAx{} -> Nothing
  PNamed{} -> Nothing -- TODO: Capaz mantenerlo
  PLEM -> Nothing
  PTrueI -> Nothing
  -- Congruencias
  p@(PImpI hypAntecedent proofConsequent) ->
    reduceCong1
      proofConsequent
      (\proofConsequent' -> p{proofConsequent = proofConsequent'})
  p@(PImpE antecedent proofImp proofAntecedent) ->
    reduceCong2
      proofImp
      (\proofImp' -> p{proofImp = proofImp'})
      proofAntecedent
      (\proofAntecedent' -> p{proofAntecedent = proofAntecedent'})
  p@(PNotI hyp proofBot) ->
    reduceCong1
      proofBot
      (\proofBot' -> p{proofBot = proofBot'})
  p@(PNotE form proofNotForm proofForm) ->
    reduceCong2
      proofNotForm
      (\proofNotForm' -> p{proofNotForm = proofNotForm'})
      proofForm
      (\proofForm' -> p{proofForm = proofForm'})
  p@(PAndI proofLeft proofRight) ->
    reduceCong2
      proofLeft
      (\proofLeft' -> p{proofLeft = proofLeft'})
      proofRight
      (\proofRight' -> p{proofRight = proofRight'})
  p@(PAndE1 right proofAnd) ->
    reduceCong1
      proofAnd
      (\proofAnd' -> p{proofAnd = proofAnd'})
  p@(PAndE2 left proofAnd) ->
    reduceCong1
      proofAnd
      (\proofAnd' -> p{proofAnd = proofAnd'})
  p@(POrI1 proofLeft) ->
    reduceCong1
      proofLeft
      (\proofLeft' -> p{proofLeft = proofLeft'})
  p@(POrI2 proofRight) ->
    reduceCong1
      proofRight
      (\proofRight' -> p{proofRight = proofRight'})
  p@(POrE left right proofOr hypLeft proofAssumingLeft hypRight proofAssumingRight) ->
    reduceCong3
      proofOr
      (\proofOr' -> p{proofOr = proofOr'})
      proofAssumingLeft
      (\proofAssumingLeft' -> p{proofAssumingLeft = proofAssumingLeft'})
      proofAssumingRight
      (\proofAssumingRight' -> p{proofAssumingRight = proofAssumingRight'})
  p@(PFalseE proofBot) ->
    reduceCong1
      proofBot
      (\proofBot' -> p{proofBot = proofBot'})
  p@(PForallE var form proofForall termReplace) ->
    reduceCong1 proofForall (\proofForall' -> p{proofForall = proofForall'})
  p -> error (show p)

reduceCong1 :: Proof -> (Proof -> Proof) -> Maybe Proof
reduceCong1 p r = do
  p' <- reduce1 p
  return (r p')

reduceCong2 ::
  Proof ->
  (Proof -> Proof) ->
  Proof ->
  (Proof -> Proof) ->
  Maybe Proof
reduceCong2 p1 r1 p2 r2 = case reduce1 p1 of
  Just p1' -> Just $ r1 p1'
  Nothing -> case reduce1 p2 of
    Just p2' -> Just $ r2 p2'
    Nothing -> Nothing

reduceCong3 ::
  Proof ->
  (Proof -> Proof) ->
  Proof ->
  (Proof -> Proof) ->
  Proof ->
  (Proof -> Proof) ->
  Maybe Proof
reduceCong3 p1 r1 p2 r2 p3 r3 = case reduceCong2 p1 r1 p2 r2 of
  Just p' -> Just p'
  Nothing -> case reduce1 p3 of
    Just p3' -> Just $ r3 p3'
    Nothing -> Nothing

-- NDReducer contiene lo necesario para poder reducir una ND.Proof
-- a una equivalente más chica.
module NDReducer (reduce, substHyp) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import ND (HypId, Proof (..), Term, VarId)

-- Sustituciones sobre demostraciones

-- Sustitución de hipótesis, usado para substHyp sin capturas
type HypSubstitution = Map.Map HypId HypId

-- Hace (subst varid term) en toda la proof recursivamente
substVar :: Proof -> VarId -> Term -> Proof
substVar = undefined

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
    } -> Just $ substHyp hypLeft proofAssumingLeft proofLeft
  POrE
    { proofOr = POrI2{proofRight = proofRight}
    , hypRight = hypRight
    , proofAssumingRight = proofAssumingRight
    } -> Just $ substHyp hypRight proofAssumingRight proofRight
  -- Valores
  PAx{} -> Nothing
  PNamed{} -> Nothing -- TODO: Capaz mantenerlo
  -- Congruencia
  PImpI hypAntecedent proofConsequent -> case reduce1 proofConsequent of
    Nothing -> Nothing
    Just p' -> Just $ PImpI hypAntecedent p'
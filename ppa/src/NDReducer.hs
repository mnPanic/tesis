-- NDReducer contiene lo necesario para poder reducir una ND.Proof
-- a una equivalente más chica.
module NDReducer (reduce) where

import Control.DeepSeq (force)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Debug.Trace (trace)
import ND (HypId, Proof (..), Term (TVar), VarId, VarSubstitution, fvTerm, proofName)
import NDSubst (substHyp, substVar)
import Text.Printf (printf)

-- freshWRT da una hyp no usada con respecto a una lista en donde no queremos
-- que aparezca
freshWRT :: (Foldable t) => HypId -> t HypId -> HypId
freshWRT h forbidden = head [h ++ suffix | suffix <- map show [0 ..], h ++ suffix `notElem` forbidden]

-- Reduce una demostración hasta que sea irreducible (big step).
-- Asume que chequea.
reduce :: Proof -> Proof
reduce p | trace (printf "reduce %s" (proofName p)) False = undefined
reduce p = maybe p reduce (reduce1 p)

-- Realiza un paso small step de reducción de la demostración
-- Devuelve Nothing si es irreducible.
reduce1 :: Proof -> Maybe Proof
reduce1 p | trace (printf "\nreduce1 %s" (show p)) False = undefined
reduce1 p = case p of
  -- Reducción de And
  -- PAndEi(PAndI(Pi_1, Pi_2)) -> PI_i
  PAndE1 right (PAndI proofLeft proofRight) -> Nothing -- Just proofLeft
  PAndE2 left (PAndI proofLeft proofRight) -> Just proofRight
  -- Reducción de Or
  -- POrE(POrI(Pi), h.proofLeft)
  POrE
    { proofOr = POrI1{proofLeft = proofLeft}
    , hypLeft = hypLeft
    , proofAssumingLeft = proofAssumingLeft
    } -> Nothing -- Just $ substHyp hypLeft proofLeft proofAssumingLeft
  POrE
    { proofOr = POrI2{proofRight = proofRight}
    , hypRight = hypRight
    , proofAssumingRight = proofAssumingRight
    } -> Nothing -- Just $ substHyp hypRight proofRight proofAssumingRight
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
    } -> Nothing -- Just $ substHyp hypForm proofForm proofBot
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
    } -> Nothing -- Just $ substVar x' t proofForm
    -- Reducción de Exists
  PExistsE
    { var = x
    , form = form
    , proofExists =
      PExistsI
        { inst = t
        , proofFormWithInst = proofFormWithInst
        }
    , hyp = hypForm
    , proofAssuming = proofAssuming
    } ->
      let proofAssumingWithInst = substVar x t proofAssuming
       in Nothing -- Just $ substHyp hypForm proofFormWithInst proofAssumingWithInst
  PNamed _ p -> Just p
  -- Valores
  PAx{} -> Nothing
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
  p@(PForallI newVar proofForm) ->
    reduceCong1 proofForm (\proofForm' -> p{proofForm = proofForm'})
  p@(PForallE var form proofForall termReplace) ->
    reduceCong1 proofForall (\proofForall' -> p{proofForall = proofForall'})
  p@(PExistsI t pInst) ->
    reduceCong1 pInst (\pInst' -> p{proofFormWithInst = pInst'})
  p@(PExistsE var form proofExists hyp proofAssuming) ->
    reduceCong2
      proofExists
      (\proofExists' -> p{proofExists = proofExists'})
      proofAssuming
      (\proofAssuming' -> p{proofAssuming = proofAssuming'})

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

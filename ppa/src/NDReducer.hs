-- NDReducer contiene lo necesario para poder reducir una ND.Proof
-- a una equivalente más chica.
module NDReducer (reduce) where

import Control.DeepSeq (force)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Debug.Trace (trace)
import ND (HypId, Proof (..), Term (TVar), VarId, VarSubstitution, fvTerm, proofName)
import NDSubst (HypMemo, substHyp, substVar)
import Text.Printf (printf)

-- freshWRT da una hyp no usada con respecto a una lista en donde no queremos
-- que aparezca
freshWRT :: (Foldable t) => HypId -> t HypId -> HypId
freshWRT h forbidden = head [h ++ suffix | suffix <- map show [0 ..], h ++ suffix `notElem` forbidden]

-- Reduce una demostración hasta que sea irreducible (big step).
-- Asume que chequea.
reduce :: Proof -> Proof
reduce = reduce' Map.empty 0

reduce' :: HypMemo -> Int -> Proof -> Proof
-- reduce' iter p | trace (printf "\n\n(%s) reduce %s" (show iter) (proofName p)) False = undefined
reduce' mem iter p = case reduce1 mem 0 p of
  Nothing -> p
  Just (mem', p') -> reduce' mem' (iter + 1) p'

indent :: Int -> String
indent n = concat $ replicate (n * 2) "  "

-- Realiza un paso small step de reducción de la demostración
-- Devuelve Nothing si es irreducible.
reduce1 :: HypMemo -> Int -> Proof -> Maybe (HypMemo, Proof)
-- reduce1 idt proof | trace (printf "%sreduce1 %s" (indent idt) (proofName proof)) False = undefined
reduce1 mem idt proof = case proof of
  -- Reducción de And
  -- PAndEi(PAndI(Pi_1, Pi_2)) -> PI_i
  PAndE1 right (PAndI proofLeft proofRight) -> Just (mem, proofLeft)
  PAndE2 left (PAndI proofLeft proofRight) -> Just (mem, proofRight)
  -- Reducción de Or
  -- POrE(POrI(Pi), h.proofLeft)
  POrE
    { proofOr = POrI1{proofLeft = proofLeft}
    , hypLeft = hypLeft
    , proofAssumingLeft = proofAssumingLeft
    } -> Just $ substHyp mem (idt + 1) hypLeft proofLeft proofAssumingLeft
  POrE
    { proofOr = POrI2{proofRight = proofRight}
    , hypRight = hypRight
    , proofAssumingRight = proofAssumingRight
    } -> Just $ substHyp mem (idt + 1) hypRight proofRight proofAssumingRight
  -- Reducción de Imp
  PImpE
    { antecedent = ant
    , proofImp =
      PImpI
        { hypAntecedent = hypAntecedent
        , proofConsequent = proofAntThenCons
        }
    , proofAntecedent = proofAnt
    } -> Just $ substHyp mem (idt + 1) hypAntecedent proofAnt proofAntThenCons
  -- Reducción de Not
  PNotE
    { form = form
    , proofNotForm =
      PNotI
        { hyp = hypForm
        , proofBot = proofBot
        }
    , proofForm = proofForm
    } -> Just $ substHyp mem (idt + 1) hypForm proofForm proofBot
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
    } -> Just $ (mem, substVar x' t proofForm)
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
       in Just $ substHyp mem (idt + 1) hypForm proofFormWithInst proofAssumingWithInst
  PNamed _ p -> Just (mem, p)
  -- Valores
  PAx{} -> Nothing
  PLEM -> Nothing
  PTrueI -> Nothing
  -- Congruencias
  p@(PImpI hypAntecedent proofConsequent) ->
    reduceCong1
      mem
      idt
      proofConsequent
      (\proofConsequent' -> p{proofConsequent = proofConsequent'})
  p@(PImpE antecedent proofImp proofAntecedent) ->
    reduceCong2
      mem
      idt
      proofImp
      (\proofImp' -> p{proofImp = proofImp'})
      proofAntecedent
      (\proofAntecedent' -> p{proofAntecedent = proofAntecedent'})
  p@(PNotI hyp proofBot) ->
    reduceCong1
      mem
      idt
      proofBot
      (\proofBot' -> p{proofBot = proofBot'})
  p@(PNotE form proofNotForm proofForm) ->
    reduceCong2
      mem
      idt
      proofNotForm
      (\proofNotForm' -> p{proofNotForm = proofNotForm'})
      proofForm
      (\proofForm' -> p{proofForm = proofForm'})
  p@(PAndI proofLeft proofRight) ->
    reduceCong2
      mem
      idt
      proofLeft
      (\proofLeft' -> p{proofLeft = proofLeft'})
      proofRight
      (\proofRight' -> p{proofRight = proofRight'})
  p@(PAndE1 right proofAnd) ->
    reduceCong1
      mem
      idt
      proofAnd
      (\proofAnd' -> p{proofAnd = proofAnd'})
  p@(PAndE2 left proofAnd) ->
    reduceCong1
      mem
      idt
      proofAnd
      (\proofAnd' -> p{proofAnd = proofAnd'})
  p@(POrI1 proofLeft) ->
    reduceCong1
      mem
      idt
      proofLeft
      (\proofLeft' -> p{proofLeft = proofLeft'})
  p@(POrI2 proofRight) ->
    reduceCong1
      mem
      idt
      proofRight
      (\proofRight' -> p{proofRight = proofRight'})
  p@(POrE left right proofOr hypLeft proofAssumingLeft hypRight proofAssumingRight) ->
    reduceCong3
      mem
      idt
      proofOr
      (\proofOr' -> p{proofOr = proofOr'})
      proofAssumingLeft
      (\proofAssumingLeft' -> p{proofAssumingLeft = proofAssumingLeft'})
      proofAssumingRight
      (\proofAssumingRight' -> p{proofAssumingRight = proofAssumingRight'})
  p@(PFalseE proofBot) ->
    reduceCong1
      mem
      idt
      proofBot
      (\proofBot' -> p{proofBot = proofBot'})
  p@(PForallI newVar proofForm) ->
    reduceCong1 mem idt proofForm (\proofForm' -> p{proofForm = proofForm'})
  p@(PForallE var form proofForall termReplace) ->
    reduceCong1 mem idt proofForall (\proofForall' -> p{proofForall = proofForall'})
  p@(PExistsI t pInst) ->
    reduceCong1 mem idt pInst (\pInst' -> p{proofFormWithInst = pInst'})
  p@(PExistsE var form proofExists hyp proofAssuming) ->
    reduceCong2
      mem
      idt
      proofExists
      (\proofExists' -> p{proofExists = proofExists'})
      proofAssuming
      (\proofAssuming' -> p{proofAssuming = proofAssuming'})

reduceCong1 :: HypMemo -> Int -> Proof -> (Proof -> Proof) -> Maybe (HypMemo, Proof)
reduceCong1 mem idt p r = do
  (mem', p') <- reduce1 mem (idt + 1) p
  return (mem', r p')

reduceCong2 ::
  HypMemo ->
  Int ->
  Proof ->
  (Proof -> Proof) ->
  Proof ->
  (Proof -> Proof) ->
  Maybe (HypMemo, Proof)
reduceCong2 mem idt p1 r1 p2 r2 = case reduce1 mem (idt + 1) p1 of
  Just (mem', p1') -> Just (mem', r1 p1')
  Nothing -> case reduce1 mem (idt + 1) p2 of
    Just (mem', p2') -> Just (mem', r2 p2')
    Nothing -> Nothing

reduceCong3 ::
  HypMemo ->
  Int ->
  Proof ->
  (Proof -> Proof) ->
  Proof ->
  (Proof -> Proof) ->
  Proof ->
  (Proof -> Proof) ->
  Maybe (HypMemo, Proof)
reduceCong3 mem idt p1 r1 p2 r2 p3 r3 = case reduceCong2 mem idt p1 r1 p2 r2 of
  Just p' -> Just p'
  Nothing -> case reduce1 mem (idt + 1) p3 of
    Just (mem', p3') -> Just (mem', r3 p3')
    Nothing -> Nothing

-- NDReducer contiene lo necesario para poder reducir una ND.Proof
-- a una equivalente más chica.
module NDReducer (reduce) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Debug.Trace (trace)
import ND (HypId, Proof (..), Term (TVar), VarId, VarSubstitution, fvTerm, proofName)
import NDSubst (HypMemo, substHyp, substVar)
import Text.Printf (printf)

-- Reduce una demostración hasta que sea irreducible (big step).
-- Asume que chequea.
reduce :: Proof -> Proof
reduce = snd . reduce'' Map.empty 0

-- Hack porque en las cong tengo que saber si devolver nothing
reduce' :: HypMemo -> Int -> Proof -> Maybe (HypMemo, Proof)
-- reduce' iter p | trace (printf "\n\n(%s) reduce %s" (show iter) (proofName p)) False = undefined
reduce' mem iter p = case reduce1 mem 0 p of
  Nothing -> Nothing
  Just (mem', p') -> Just $ reduce'' mem' (iter + 1) p'

reduce'' :: HypMemo -> Int -> Proof -> (HypMemo, Proof)
reduce'' mem iter p = case reduce1 mem 0 p of
  Nothing -> (mem, p)
  Just (mem', p') -> reduce'' mem' (iter + 1) p'

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
      proofAntecedent
      (\proofImp' proofAntecedent' -> p{proofImp = proofImp', proofAntecedent = proofAntecedent'})
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
      proofForm
      (\proofNotForm' proofForm' -> p{proofNotForm = proofNotForm', proofForm = proofForm'})
  p@(PAndI proofLeft proofRight) ->
    reduceCong2
      mem
      idt
      proofLeft
      proofRight
      (\proofLeft' proofRight' -> p{proofRight = proofRight', proofLeft = proofLeft'})
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
      proofAssumingLeft
      proofAssumingRight
      (\proofOr' proofAssumingLeft' proofAssumingRight' -> p{proofOr = proofOr', proofAssumingLeft = proofAssumingLeft', proofAssumingRight = proofAssumingRight'})
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
      proofAssuming
      (\proofExists' proofAssuming' -> p{proofExists = proofExists', proofAssuming = proofAssuming'})

reduceCong1 :: HypMemo -> Int -> Proof -> (Proof -> Proof) -> Maybe (HypMemo, Proof)
reduceCong1 mem idt p r = do
  (mem', p') <- reduce' mem (idt + 1) p
  return (mem', r p')

reduceCong2 ::
  HypMemo ->
  Int ->
  Proof ->
  Proof ->
  (Proof -> Proof -> Proof) ->
  Maybe (HypMemo, Proof)
reduceCong2 mem idt p1 p2 r = case reduce' mem (idt + 1) p1 of
  Just (mem1, p1') -> case reduce' mem1 (idt + 1) p2 of
    Just (mem2, p2') -> Just (mem2, r p1' p2')
    Nothing -> Just (mem1, r p1' p2)
  Nothing -> case reduce' mem (idt + 1) p2 of
    Just (mem', p2') -> Just (mem', r p1 p2')
    Nothing -> Nothing

reduceCong2NoRep ::
  HypMemo ->
  Int ->
  Proof ->
  Proof ->
  Maybe (HypMemo, Proof, Proof)
reduceCong2NoRep mem idt p1 p2 = case reduce' mem (idt + 1) p1 of
  Just (mem1, p1') -> case reduce' mem1 (idt + 1) p2 of
    Just (mem2, p2') -> Just (mem2, p1', p2')
    Nothing -> Just (mem1, p1', p2)
  Nothing -> case reduce' mem (idt + 1) p2 of
    Just (mem1, p2') -> Just (mem1, p1, p2')
    Nothing -> Nothing

reduceCong3 ::
  HypMemo ->
  Int ->
  Proof ->
  Proof ->
  Proof ->
  (Proof -> Proof -> Proof -> Proof) ->
  Maybe (HypMemo, Proof)
reduceCong3 mem idt p1 p2 p3 r =
  case reduceCong2NoRep mem idt p1 p2 of
    Just (mem1, p1', p2') -> case reduce' mem1 (idt + 1) p3 of
      Just (mem2, p3') -> Just (mem2, r p1' p2' p3')
      Nothing -> Nothing
    Nothing -> case reduce' mem (idt + 1) p3 of
      Just (mem1, p3') -> Just (mem1, r p1 p2 p3')
      Nothing -> Nothing

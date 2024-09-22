-- NDReducer contiene lo necesario para poder reducir una ND.Proof
-- a una equivalente más chica.
module NDReducer (reduce) where

import ND (Proof (..))
import NDSubst (substHyp, substVar)

-- Reduce una demostración hasta que sea irreducible (big step).
-- Asume que chequea.
reduce :: Proof -> Proof
reduce p = maybe p reduce (reduce1 p)

-- Hack porque en las cong tengo que saber si devolver nothing
reduce' :: Proof -> Maybe Proof
reduce' p = case reduce1 p of
  Nothing -> Nothing
  Just p' -> Just $ reduce p'

-- Realiza un paso small step de reducción de la demostración
-- Devuelve Nothing si es irreducible.
reduce1 :: Proof -> Maybe Proof
reduce1 proof = case proof of
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
       in Just $ substHyp hypForm proofFormWithInst proofAssumingWithInst
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
      proofAntecedent
      (\proofImp' proofAntecedent' -> p{proofImp = proofImp', proofAntecedent = proofAntecedent'})
  p@(PNotI hyp proofBot) ->
    reduceCong1
      proofBot
      (\proofBot' -> p{proofBot = proofBot'})
  p@(PNotE form proofNotForm proofForm) ->
    reduceCong2
      proofNotForm
      proofForm
      (\proofNotForm' proofForm' -> p{proofNotForm = proofNotForm', proofForm = proofForm'})
  p@(PAndI proofLeft proofRight) ->
    reduceCong2
      proofLeft
      proofRight
      (\proofLeft' proofRight' -> p{proofRight = proofRight', proofLeft = proofLeft'})
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
      proofAssumingLeft
      proofAssumingRight
      (\proofOr' proofAssumingLeft' proofAssumingRight' -> p{proofOr = proofOr', proofAssumingLeft = proofAssumingLeft', proofAssumingRight = proofAssumingRight'})
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
      proofAssuming
      (\proofExists' proofAssuming' -> p{proofExists = proofExists', proofAssuming = proofAssuming'})

reduceCong1 :: Proof -> (Proof -> Proof) -> Maybe Proof
reduceCong1 p r = do
  p' <- reduce' p
  return (r p')

reduceCong2 ::
  Proof ->
  Proof ->
  (Proof -> Proof -> Proof) ->
  Maybe Proof
reduceCong2 p1 p2 r = case reduceCong2NoRep p1 p2 of
  Just (p1', p2') -> Just (r p1' p2')
  Nothing -> Nothing

reduceCong2NoRep ::
  Proof ->
  Proof ->
  Maybe (Proof, Proof)
reduceCong2NoRep p1 p2 = case reduce' p1 of
  Just p1' -> case reduce' p2 of
    Just p2' -> Just (p1', p2')
    Nothing -> Just (p1', p2)
  Nothing -> case reduce' p2 of
    Just p2' -> Just (p1, p2')
    Nothing -> Nothing

reduceCong3 ::
  Proof ->
  Proof ->
  Proof ->
  (Proof -> Proof -> Proof -> Proof) ->
  Maybe Proof
reduceCong3 p1 p2 p3 r =
  case reduceCong2NoRep p1 p2 of
    Just (p1', p2') -> case reduce' p3 of
      Just p3' -> Just (r p1' p2' p3')
      Nothing -> Nothing
    Nothing -> case reduce' p3 of
      Just p3' -> Just (r p1 p2 p3')
      Nothing -> Nothing

module Extractor.Extractor (
  extractWitnessCtx,
  inlineAxioms,
) where

import Extractor.RProofs (
  transIntro,
 )
import Extractor.Reducer (reduce)
import Extractor.Translator.Proof (translateFriedman)
import Extractor.Types (R, fromPi02, toPi02)
import ND.ND (Form (..), HypId, Proof (..), Term (TVar), proofName)
import ND.Subst (subst, substHyp)
import PPA.Certifier (checkContext)
import PPA.PPA (Context, Hypothesis (HAxiom, HTheorem), axioms, findHyp, removeHyp)
import PPA.Proofs (Result, wrapR)
import Text.Printf (printf)

{-
Para mantener los axiomas originales, reemplazo ax: f las ocurrencias de ax por
una dem de f~~ a partir de f
-}
extractWitnessCtx :: Context -> HypId -> [Term] -> Result (Context, Term, Form)
extractWitnessCtx ctx theoremId terms = do
  h_theorem <- findHyp ctx theoremId
  case h_theorem of
    HAxiom{} -> Left $ printf "%s is an axiom, not a theorem" theoremId
    HTheorem h f p -> do
      let ctxRest = removeHyp ctx theoremId
      let pInlined = inlineProofs ctxRest p
      wrapR "check inlined" $ checkContext (axioms ctx ++ [HTheorem h f pInlined])

      (pInlinedTranslated, f_exists, t) <- extractWitness (axioms ctx) pInlined f terms
      let (FExists x f') = f_exists

      let ctxResult = axioms ctx ++ [HTheorem h f_exists pInlinedTranslated]

      let f_witnessed = subst x t f'
      return (ctxResult, t, f_witnessed)

-- Inlinea todos los proofs del context en el proof
inlineProofs :: Context -> Proof -> Proof
inlineProofs ctx p = foldr inlineHyp p ctx
 where
  inlineHyp hyp proof = case hyp of
    HAxiom{} -> proof
    HTheorem h _ p_f -> substHyp h (PNamed (printf "theorem %s" h) p_f) proof

inlineAxioms :: Context -> Proof -> R -> Proof
inlineAxioms ctx p r = foldr inlineAxiom p ctx
 where
  inlineAxiom hyp proof = case hyp of
    HTheorem{} -> proof
    HAxiom h f ->
      substHyp
        h
        ( PNamed
            (printf "axiom %s translation intro" h)
            (transIntro f h r)
        )
        proof

{- Dada una demostración de forall Y. exists X . p(Y, X), dada una instancia de Y:=a,
 devuelve t tal que p(a, t).

Para ello,

- traduce la demostración de clásica a intuicionista usando la traducción de
  Friedman (da una demostración intuicionista de forall Y . exists X . p(X))
- inlinea traducción de los axiomas
- usa la demostración para demostrar el goal instanciando los foralls (exists X . p(a, X))
- chequea la demostración resultante
- reduce la demostración
- en la forma normal, debería comenzar con un PExistsI

La fórmula debe ser de la clase Sigma^0_1, es decir N
existenciales seguidos de una fórmula sin cuantificadores.
-}
extractWitness :: Context -> Proof -> Form -> [Term] -> Result (Proof, Form, Term)
extractWitness ctxAxioms proof form instanceTerms = do
  formPi02@(ys, f_exists) <- toPi02 form
  if length ys /= length instanceTerms
    then Left $ printf "need to instantiate forall vars: '%s', but got different number of terms to instantiate: '%s'" (show ys) (show instanceTerms)
    else do
      let (proofNJ, r) = translateFriedman proof formPi02

      -- proofNJ asume que los axiomas están traducidos. Para que se mantengan,
      -- demostramos que los axiomas originales implican su traducción.
      let proofNJAdaptedAxioms = inlineAxioms ctxAxioms proofNJ r

      -- Queremos demostrar la fórmula con los foralls instanciados, para que
      -- la fórmula final sea un exists y la demostración normalizada arranque
      -- con PExistsI.
      -- Como proofNJ es una dem de la fórmula original, con los foralls, la
      -- usamos para demostrar solo el exists.
      let f_exists_inst = foldr (\(y, t) f -> subst y t f) f_exists (zip ys instanceTerms)
      let (proofInst, _) =
            foldr
              ( \t (subProof, (x : xs, subForm)) ->
                  ( PForallE
                      { var = x
                      , form = fromPi02 (xs, subForm)
                      , termReplace = t
                      , proofForall = subProof
                      }
                  , (xs, subst x t subForm)
                  )
              )
              (proofNJAdaptedAxioms, formPi02)
              (reverse instanceTerms)
      wrapR "check pre-reduce" $ checkContext (ctxAxioms ++ [HTheorem "h" f_exists_inst proofInst])

      let reducedProof = reduce proofInst
      case reducedProof of
        (PExistsI t _) -> return (reducedProof, f_exists_inst, t)
        proof' -> return (reducedProof, f_exists_inst, TVar $ printf "(broken) %s not ExistsI" (proofName proof'))

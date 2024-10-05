module PrettyShow where

import Data.List (intercalate)
import ND (Form, Proof (..), Term)
import PPA (Context, Hypothesis (HAxiom, HTheorem))
import Text.Printf (printf)

class PrettyShow a where
    prettyShow :: a -> String

instance PrettyShow Form where
    prettyShow = show

instance PrettyShow Term where
    prettyShow = show

instance PrettyShow Context where
    prettyShow hs = printf "[\n%s]" hyps
      where
        hyps = intercalate ",\n" (map (indent 1 . prettyShow) hs)

indent :: Int -> String -> String
indent n s = unlines $ map ((concat $ replicate n "  ") ++) (lines s)

instance PrettyShow Hypothesis where
    prettyShow (HAxiom h f) = printf "(axiom) %s : %s" h (prettyShow f)
    prettyShow (HTheorem h f p) = printf "(theorem) %s : %s . Proof =\n %s" h (prettyShow f) (prettyShow p)

instance PrettyShow Proof where
    prettyShow proof = case proof of
        PNamed n prf ->
            printf "PNamed {\n  name = %s,\n  proof =%s\n}" n (formatSubProof prf)
        PAx h ->
            printf "PAx %s" h
        PAndI{proofLeft = pL, proofRight = pR} ->
            printf
                "PAndI {\n  proofLeft =%s,\n  proofRight =%s\n}"
                (formatSubProof pL)
                (formatSubProof pR)
        PAndE1{right = r, proofAnd = p} ->
            printf
                "PAndE1 {\n  right = %s,\n  proofAnd =%s\n}"
                (prettyShow r)
                (formatSubProof p)
        PAndE2{left = l, proofAnd = p} ->
            printf
                "PAndE2 {\n  left = %s,\n  proofAnd =%s\n}"
                (prettyShow l)
                (formatSubProof p)
        POrI1{proofLeft = pL} ->
            printf "POrI1 {\n  proofLeft =%s\n}" (formatSubProof pL)
        POrI2{proofRight = pR} ->
            printf "POrI2 {\n  proofRight =%s\n}" (formatSubProof pR)
        POrE{left = l, right = r, proofOr = p, hypLeft = hL, proofAssumingLeft = pL, hypRight = hR, proofAssumingRight = pR} ->
            printf
                "POrE {\n  left = %s,\n  right = %s,\n  proofOr =%s,\n  hypLeft = %s,\n  proofAssumingLeft =%s,\n  hypRight = %s,\n  proofAssumingRight =%s\n}"
                (prettyShow l)
                (prettyShow r)
                (formatSubProof p)
                hL
                (formatSubProof pL)
                hR
                (formatSubProof pR)
        PImpI{hypAntecedent = h, proofConsequent = p} ->
            printf "PImpI {\n  hypAntecedent = %s,\n  proofConsequent =%s\n}" h (formatSubProof p)
        PImpE{antecedent = a, proofImp = pImp, proofAntecedent = pA} ->
            printf
                "PImpE {\n  antecedent = %s,\n  proofImp =%s,\n  proofAntecedent =%s\n}"
                (prettyShow a)
                (formatSubProof pImp)
                (formatSubProof pA)
        PNotI{hyp = h, proofBot = p} ->
            printf "PNotI {\n  hyp = %s,\n  proofBot =%s\n}" h (formatSubProof p)
        PNotE{form = f, proofNotForm = pN, proofForm = pF} ->
            printf
                "PNotE {\n  form = %s,\n  proofNotForm =%s,\n  proofForm =%s\n}"
                (prettyShow f)
                (formatSubProof pN)
                (formatSubProof pF)
        PTrueI ->
            "PTrueI"
        PFalseE{proofBot = p} ->
            printf "PFalseE {\n  proofBot =%s\n}" (formatSubProof p)
        PLEM ->
            "PLEM"
        PForallI{newVar = v, proofForm = p} ->
            printf "PForallI {\n  newVar = %s,\n  proofForm =%s\n}" v (formatSubProof p)
        PForallE{var = v, form = f, proofForall = p, termReplace = t} ->
            printf
                "PForallE {\n  var = %s,\n  form = %s,\n  proofForall =%s,\n  termReplace = %s\n}"
                v
                (prettyShow f)
                (formatSubProof p)
                (prettyShow t)
        PExistsI{inst = t, proofFormWithInst = p} ->
            printf "PExistsI {\n  inst = %s,\n  proofFormWithInst =%s\n}" (prettyShow t) (formatSubProof p)
        PExistsE{var = v, form = f, proofExists = p, hyp = h, proofAssuming = pAssume} ->
            printf
                "PExistsE {\n  var = %s,\n  form = %s,\n  proofExists =%s,\n  hyp = %s,\n  proofAssuming =%s\n}"
                v
                (prettyShow f)
                (formatSubProof p)
                h
                (formatSubProof pAssume)

-- Helper function to format sub-proofs
formatSubProof :: Proof -> String
formatSubProof proof =
    let formattedProof = prettyShow proof
     in if length (lines formattedProof) > 1
            then "\n" ++ indent 2 formattedProof
            else " " ++ formattedProof

-- Contains proof macros / generators
module NDProofs (
    proofAndEProjection,
    cut,
    proofImpElim,
    hypForm,
    doubleNegElim,
    Result,
    EnvItem,
) where

import ND (
    Form (..),
    HypId,
    Proof (..),
 )

import Text.Printf (printf)

type Result a = Either String a

-- (x: A) es un elemento del entorno de una demostración.
-- Usado por todas las funciones que generan demostraciones del estilo
--  x: A |- B
-- para especificar x:A
type EnvItem = (HypId, Form)

{- doubleNegElim dada una fórmula A da una demostración de ~~A -> A

Se puede usar para demostrar A por contradicción, asumiendo ~A
    PImpE
        dneg A
        dnegElim A
        -- Proof de ~~A
        PNotI "h ~A"
            -- Proof de bottom asumiendo ~A, es decir que asumir que no vale A
            -- lleva a una contradicción.
-}
doubleNegElim :: Form -> Proof
doubleNegElim formA =
    PImpI
        "h ~~{A}"
        ( -- Uso LEM de A v ~A
          POrE
            formA
            (FNot formA)
            PLEM
            -- Dem de A asumiendo A
            "h {A}"
            (PAx "h {A}")
            -- Dem de A asumiendo ~ A
            "h ~{A}"
            ( -- ~A y ~~A generan una contradicción
              PFalseE
                ( PNotE
                    (FNot formA) -- Uso ~~A
                    -- Dem de ~~A
                    (PAx "h ~~{A}")
                    -- Dem de ~A
                    (PAx "h ~{A}")
                )
            )
        )

{- cut es un macro que permite pegar demostraciones

    G |- A      G, A |- B
    --------------------- (cut)
           G |- B

Permite evitar Imp-E y Imp-I para demostrar a partir de una implicación conocida
-}
cut :: Form -> Proof -> HypId -> Proof -> Proof
cut fA pA hypA pAtoB =
    PImpE
        { antecedent = fA
        , proofImp =
            PImpI
                { hypAntecedent = hypA
                , proofConsequent = pAtoB
                }
        , proofAntecedent = pA
        }

{- proofAndEProjection
Dada una cláusula (a_1 ^ ... ^ a_n), que puede estar asociada de cualquier
manera, devuelve una demostración de

    (a_1 ^ ... ^ a_n) |- a_i

con i de 1 a n

Implementación: lo raro es que las demostraciones son "al revés", por ej. con

    (a ^ b) ^ c |- b

comienza con PAndE2 del (a ^ b) y luego hay que demostrar de donde sale ese and
con PAndE1. Para resolverlo, se usa una función auxiliar que recorre el and
estructuralmente y devuelve errores si no matchea o id si lo encuentra. Luego se
rellena con E1 o E2 según el camino tomado y devuelve una demostración a medias,
que dice cual se usa (E1 o E2) pero no demuestra el and, ya que eso sabe cómo
hacerlo el padre (recursivamente con E1 o E2).
-}
proofAndEProjection :: EnvItem -> Form -> Result Proof
proofAndEProjection (hAnd, fAnd) f =
    case proofAndEProjection' fAnd f of
        Right p -> Right $ p (PAx hAnd)
        Left e -> Left e

-- Devuelve una demostración para l ^ r |- f sin demostrar el and.
proofAndEProjection' :: Form -> Form -> Result (Proof -> Proof)
proofAndEProjection' fAnd@(FAnd l r) f
    | fAnd == f = Right id
    | otherwise = case proofAndEProjection' l f of
        Right p -> Right $ \next -> p (PAndE1 r next)
        Left el -> case proofAndEProjection' r f of
            Left er ->
                Left
                    $ printf
                        "%s |- %s not possible by left (%s) or right (%s)"
                        (show fAnd)
                        (show f)
                        el
                        er
            Right p -> Right $ \next -> p (PAndE2 l next)
proofAndEProjection' f1 f2
    | f1 == f2 = Right id
    | otherwise = Left $ printf "%s /= %s" (show f1) (show f2)

-- DeMorgan y transformaciones para DNF

-- Da una demostración para X => Y |- ~X v Y y ~X v Y |- X => Y
proofImpElim :: Form -> Form -> HypId -> HypId -> (Proof, Proof)
proofImpElim x y hImp hOr =
    (proofImpElim, proofOrToImp)
  where
    -- X => Y |- ~X v Y
    -- Usando LEM, si vale X entonces vale Y. Si no vale X, vale ~X
    proofImpElim =
        POrE
            { left = x
            , right = FNot x
            , proofOr = PLEM
            , hypLeft = hX
            , proofAssumingLeft =
                POrI2
                    { proofRight =
                        PImpE
                            { antecedent = x
                            , proofImp = PAx hImp
                            , proofAntecedent = PAx hX
                            }
                    }
            , hypRight = hNotX
            , proofAssumingRight =
                POrI1
                    { proofLeft = PAx hNotX
                    }
            }
    -- ~X v Y |- X => Y
    proofOrToImp =
        PImpI
            { hypAntecedent = hX
            , proofConsequent =
                POrE
                    { left = FNot x
                    , right = y
                    , proofOr = PAx hOr
                    , -- Si vale ~X, como ya tenemos de hip X llegamos a un abs
                      hypLeft = hNotX
                    , proofAssumingLeft =
                        PFalseE
                            { proofBot =
                                PNotE
                                    { form = x
                                    , proofNotForm = PAx hNotX
                                    , proofForm = PAx hX
                                    }
                            }
                    , -- Si vale Y es trivial probar Y
                      hypRight = hY
                    , proofAssumingRight = PAx hY
                    }
            }
    hX = hypForm x
    hY = hypForm y
    hNotX = hypForm $ FNot x

hypForm :: Form -> HypId
hypForm f = "h " ++ show f
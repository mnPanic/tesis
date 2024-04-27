-- Contains proof macros / generators
module NDProofs (
    proofAndEProjection,
    cut,
    proofImpElim,
    hypForm,
    proofNotTrue,
    proofNotFalse,
    proofAndAssoc,
    proofOrAssoc,
    doubleNegElim,
    proofDNegElim,
    proofAndDistOverOrL,
    proofAndDistOverOrR,
    proofNotDistOverAnd,
    proofNotDistOverOr,
    proofAndCongruence1,
    proofAndCongruence2,
    proofOrCongruence1,
    proofOrCongruence2,
    proofNotCongruence,
    proofImpCongruence1,
    proofImpCongruence2,
    proofAndIList,
    Result,
    wrapR,
    EnvItem,
) where

import ND (
    Form (..),
    HypId,
    Proof (..),
    dneg,
 )

import Text.Printf (printf)

type Result a = Either String a

wrapR :: String -> Result a -> Result a
wrapR _ r@(Right _) = r
wrapR msg (Left err) = Left (msg ++ ": " ++ err)

-- (x: A) es un elemento del entorno de una demostración.
-- Usado por todas las funciones que generan demostraciones del estilo
--  x: A |- B
-- para especificar x:A
type EnvItem = (HypId, Form)

-- Genera un nombre para una hipótesis a partir de una fórmula
-- El prefijo __h es una forma best-effort de evitar colisiones con los hypID de
-- los usuarios
hypForm :: Form -> HypId
hypForm f = "__h " ++ show f

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
        hNotNotA
        ( -- Uso LEM de A v ~A
          POrE
            formA
            (FNot formA)
            PLEM
            -- Dem de A asumiendo A
            hA
            (PAx hA)
            -- Dem de A asumiendo ~ A
            hNotA
            ( -- ~A y ~~A generan una contradicción
              PFalseE
                ( PNotE
                    (FNot formA) -- Uso ~~A
                    -- Dem de ~~A
                    (PAx hNotNotA)
                    -- Dem de ~A
                    (PAx hNotA)
                )
            )
        )
  where
    hA = hypForm formA
    hNotA = hypForm $ FNot formA
    hNotNotA = hypForm $ dneg formA

{- cut es un macro que permite pegar demostraciones

    G |- A      G, A |- B
    --------------------- (cut)
           G |- B

Permite evitar Imp-E y Imp-I para demostrar a partir de una implicación conocida
(A -> B)
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

{- proofAndIList

Dada una lista de demostraciones [p1, ..., pn] que demuestran (a1, ..., an)
devuelve una demostración de a1 ^ ... ^ an asociada a izquierda.
-}
proofAndIList :: [Proof] -> Proof
proofAndIList [p] = p
proofAndIList ps =
    PAndI
        { proofLeft = proofAndIList (init ps)
        , proofRight = last ps
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

-------------------- DeMorgan y transformaciones para DNF ----------------------

-- Dem de x ^ (y v z) -|- (x ^ y) v (x ^ z)
proofAndDistOverOrL :: Form -> Form -> Form -> HypId -> HypId -> (Proof, Proof)
proofAndDistOverOrL x y z hAnd hOr =
    ( PNamed "and dist over or L (LR)" proofAndToOr
    , PNamed "and dist over or L (RL)" proofOrToAnd
    )
  where
    -- x ^ (y v z) |- (x ^ y) v (x ^ z)
    proofAndToOr =
        POrE
            { left = y
            , right = z
            , proofOr =
                PAndE2
                    { left = x
                    , proofAnd = PAx hAnd
                    }
            , hypLeft = hY
            , proofAssumingLeft =
                POrI1
                    { proofLeft =
                        -- x ^ y
                        PAndI
                            { proofLeft =
                                PAndE1
                                    { right = FOr y z
                                    , proofAnd = PAx hAnd
                                    }
                            , proofRight = PAx hY
                            }
                    }
            , hypRight = hZ
            , proofAssumingRight =
                POrI2
                    { proofRight =
                        -- x ^ z
                        PAndI
                            { proofLeft =
                                PAndE1
                                    { right = FOr y z
                                    , proofAnd = PAx hAnd
                                    }
                            , proofRight = PAx hZ
                            }
                    }
            }
    hY = hypForm y
    hZ = hypForm z
    hYOrZ = hypForm (FOr y z)

    -- (x ^ y) v (x ^ z) |- x ^ (y v z)
    proofOrToAnd =
        PAndI
            { -- x
              proofLeft =
                POrE
                    { left = FAnd x y
                    , right = FAnd x z
                    , proofOr = PAx hOr
                    , hypLeft = hXAndY
                    , proofAssumingLeft =
                        PAndE1
                            { right = y
                            , proofAnd = PAx hXAndY
                            }
                    , hypRight = hXAndZ
                    , proofAssumingRight =
                        PAndE1
                            { right = z
                            , proofAnd = PAx hXAndZ
                            }
                    }
            , -- y v z
              proofRight =
                POrE
                    { left = FAnd x y
                    , right = FAnd x z
                    , proofOr = PAx hOr
                    , hypLeft = hXAndY
                    , proofAssumingLeft =
                        POrI1
                            { proofLeft =
                                PAndE2
                                    { left = x
                                    , proofAnd = PAx hXAndY
                                    }
                            }
                    , hypRight = hXAndZ
                    , proofAssumingRight =
                        POrI2
                            { proofRight =
                                PAndE2
                                    { left = x
                                    , proofAnd = PAx hXAndZ
                                    }
                            }
                    }
            }
    hXAndY = hypForm (FAnd x y)
    hXAndZ = hypForm (FAnd x z)

-- Dem de (y v z) ^ x -|- (y ^ x) v (z ^ x)
proofAndDistOverOrR :: Form -> Form -> Form -> HypId -> HypId -> (Proof, Proof)
proofAndDistOverOrR x y z hAnd hOr =
    ( PNamed "and dist over or R (LR)" proofAndToOr
    , PNamed "and dist over or R (RL)" proofOrToAnd
    )
  where
    -- (y v z) ^ x |- (y ^ x) v (z ^ x)
    proofAndToOr =
        POrE
            { left = y
            , right = z
            , proofOr =
                PAndE1
                    { right = x
                    , proofAnd = PAx hAnd
                    }
            , hypLeft = hY
            , -- pruebo (y ^ x)
              proofAssumingLeft =
                POrI1
                    { proofLeft =
                        PAndI
                            { proofLeft = PAx hY
                            , proofRight =
                                PAndE2
                                    { left = FOr y z
                                    , proofAnd = PAx hAnd
                                    }
                            }
                    }
            , -- pruebo (z ^ x)
              hypRight = hZ
            , proofAssumingRight =
                POrI2
                    { proofRight =
                        PAndI
                            { proofLeft = PAx hZ
                            , proofRight =
                                PAndE2
                                    { left = FOr y z
                                    , proofAnd = PAx hAnd
                                    }
                            }
                    }
            }
    (hY, hZ) = (hypForm y, hypForm z)
    -- (y ^ x) v (z ^ x) |- (y v z) ^ x
    proofOrToAnd =
        POrE
            { left = FAnd y x
            , right = FAnd z x
            , proofOr = PAx hOr
            , hypLeft = hYAndX
            , proofAssumingLeft =
                PAndI
                    { proofLeft =
                        POrI1
                            { proofLeft =
                                PAndE1
                                    { right = x
                                    , proofAnd = PAx hYAndX
                                    }
                            }
                    , proofRight =
                        PAndE2
                            { left = y
                            , proofAnd = PAx hYAndX
                            }
                    }
            , hypRight = hZAndX
            , proofAssumingRight =
                PAndI
                    { proofLeft =
                        POrI2
                            { proofRight =
                                PAndE1
                                    { right = x
                                    , proofAnd = PAx hZAndX
                                    }
                            }
                    , proofRight =
                        PAndE2
                            { left = z
                            , proofAnd = PAx hZAndX
                            }
                    }
            }
    hYAndX = hypForm (FAnd y x)
    hZAndX = hypForm (FAnd z x)

-- Da una dem para ~T -|- F
proofNotTrue :: HypId -> HypId -> (Proof, Proof)
proofNotTrue hNotTrue hFalse =
    ( PNamed "not true then false" pNotTrueThenFalse
    , PNamed "false then not true" pFalseThenNotTrue
    )
  where
    -- ~T |- F
    pNotTrueThenFalse =
        PNotE
            { form = FTrue
            , proofNotForm = PAx hNotTrue
            , proofForm = PTrueI
            }
    -- F |- ~T
    pFalseThenNotTrue =
        PNotI
            { hyp = hypForm FTrue
            , proofBot = PAx hFalse
            }

-- Da una dem para ~F -|- T
proofNotFalse :: HypId -> HypId -> (Proof, Proof)
proofNotFalse hNotFalse hTrue = (pNotFalseThenTrue, pTrueThenNotFalse)
  where
    -- ~F |- T
    pNotFalseThenTrue = PTrueI
    -- T |- ~F
    pTrueThenNotFalse =
        PNotI
            { hyp = hypForm FFalse
            , proofBot = PAx (hypForm FFalse)
            }

-- Da una dem para (x ^ y) ^ z -|- x ^ (y ^ z)
proofAndAssoc :: Form -> Form -> Form -> HypId -> HypId -> (Proof, Proof)
proofAndAssoc x y z hAndL hAndR =
    ( PNamed "and assoc L to R" proofAndAssocLToR
    , PNamed "and assoc R to L" proofAndAssocRToL
    )
  where
    -- (x ^ y) ^ z |- x ^ (y ^ z)
    proofAndAssocLToR =
        PAndI
            { -- x
              proofLeft =
                PAndE1
                    { right = y
                    , proofAnd =
                        PAndE1
                            { right = z
                            , proofAnd = PAx hAndL
                            }
                    }
            , -- y ^ z
              proofRight =
                PAndI
                    { proofLeft =
                        PAndE2
                            { left = x
                            , proofAnd =
                                PAndE1
                                    { right = z
                                    , proofAnd = PAx hAndL
                                    }
                            }
                    , proofRight =
                        PAndE2
                            { left = FAnd x y
                            , proofAnd = PAx hAndL
                            }
                    }
            }
    -- x ^ (y ^ z) |- (x ^ y) ^ z
    proofAndAssocRToL =
        PAndI
            { -- x ^ y
              proofLeft =
                PAndI
                    { proofLeft =
                        PAndE1
                            { right = FAnd y z
                            , proofAnd = PAx hAndR
                            }
                    , proofRight =
                        PAndE1
                            { right = z
                            , proofAnd =
                                PAndE2
                                    { left = x
                                    , proofAnd = PAx hAndR
                                    }
                            }
                    }
            , -- z
              proofRight =
                PAndE2
                    { left = y
                    , proofAnd =
                        PAndE2
                            { left = x
                            , proofAnd = PAx hAndR
                            }
                    }
            }

-- Da una dem para (x v y) v z -|- x v (y v z)
proofOrAssoc :: Form -> Form -> Form -> HypId -> HypId -> (Proof, Proof)
proofOrAssoc x y z hOrL hOrR =
    ( PNamed "or assoc L to R" proofOrAssocLToR
    , PNamed "or assoc R to L" proofOrAssocRToL
    )
  where
    -- (x v y) v z |- x v (y v z)
    proofOrAssocLToR =
        POrE
            { left = FOr x y
            , right = z
            , proofOr = PAx hOrL
            , hypLeft = hXOrY
            , proofAssumingLeft =
                POrE
                    { left = x
                    , right = y
                    , proofOr = PAx hXOrY
                    , hypLeft = hX
                    , proofAssumingLeft = POrI1{proofLeft = PAx hX}
                    , hypRight = hY
                    , proofAssumingRight =
                        POrI2
                            { proofRight = POrI1{proofLeft = PAx hY}
                            }
                    }
            , hypRight = hZ
            , proofAssumingRight =
                POrI2
                    { proofRight = POrI2{proofRight = PAx hZ}
                    }
            }
    hXOrY = hypForm $ FOr x y
    hZ = hypForm z
    hX = hypForm x
    hY = hypForm y
    -- x v (y v z) |- (x v y) v z
    proofOrAssocRToL =
        POrE
            { left = x
            , right = FOr y z
            , proofOr = PAx hOrR
            , hypLeft = hX
            , proofAssumingLeft =
                POrI1
                    { proofLeft =
                        POrI1
                            { proofLeft = PAx hX
                            }
                    }
            , hypRight = hYOrZ
            , proofAssumingRight =
                POrE
                    { left = y
                    , right = z
                    , proofOr = PAx hYOrZ
                    , hypLeft = hY
                    , proofAssumingLeft =
                        POrI1
                            { proofLeft =
                                POrI2
                                    { proofRight =
                                        PAx hY
                                    }
                            }
                    , hypRight = hZ
                    , proofAssumingRight = POrI2{proofRight = PAx hZ}
                    }
            }
    hYOrZ = hypForm $ FOr y z

-- Da una dem para ~(x v y) -|- ~x ^ ~y
proofNotDistOverOr :: Form -> Form -> HypId -> HypId -> (Proof, Proof)
proofNotDistOverOr x y hNotOr hAnd =
    ( PNamed "not dist over or LR" proofNotDistOverOrLR
    , PNamed "not dist over or RL" proofNotDistOverOrRL
    )
  where
    -- ~(x v y) |- ~x ^ ~y
    proofNotDistOverOrLR =
        PAndI
            { proofLeft =
                PNotI
                    { hyp = hX
                    , proofBot =
                        PNotE
                            { form = FOr x y
                            , proofNotForm = PAx hNotOr
                            , proofForm =
                                POrI1
                                    { proofLeft = PAx hX
                                    }
                            }
                    }
            , proofRight =
                PNotI
                    { hyp = hY
                    , proofBot =
                        PNotE
                            { form = FOr x y
                            , proofNotForm = PAx hNotOr
                            , proofForm =
                                POrI2
                                    { proofRight = PAx hY
                                    }
                            }
                    }
            }
      where
        hX = hypForm x
        hY = hypForm y

    -- ~x ^ ~y |- ~(x v y)
    proofNotDistOverOrRL =
        PNotI
            { hyp = hXOrY
            , -- Contradicción entre ~x ^ ~y, x v y depende de cual valga de or
              proofBot =
                POrE
                    { left = x
                    , right = y
                    , proofOr = PAx hXOrY
                    , hypLeft = hX
                    , proofAssumingLeft =
                        PNotE
                            { form = x
                            , proofForm = PAx hX
                            , proofNotForm =
                                PAndE1
                                    { right = FNot y
                                    , proofAnd = PAx hAnd
                                    }
                            }
                    , hypRight = hY
                    , proofAssumingRight =
                        PNotE
                            { form = y
                            , proofForm = PAx hY
                            , proofNotForm =
                                PAndE2
                                    { left = FNot x
                                    , proofAnd = PAx hAnd
                                    }
                            }
                    }
            }
      where
        hXOrY = hypForm (FOr x y)
        hX = hypForm x
        hY = hypForm y

-- Da una demostración para ~(X ^ Y) -|- ~X v ~Y
proofNotDistOverAnd :: Form -> Form -> HypId -> HypId -> (Proof, Proof)
proofNotDistOverAnd x y hNotAnd hOr =
    ( PNamed "not dist over and LR" proofNotDistOverAndLR
    , PNamed "not dist over and RL" proofNotDistOverAndRL
    )
  where
    -- ~(X ^ Y) |- ~X v ~Y
    -- No vale en intuicionista. Mega rara
    proofNotDistOverAndLR =
        PImpE
            { antecedent = dneg fOrNot
            , proofImp = doubleNegElim fOrNot
            , proofAntecedent =
                PNotI
                    { hyp = hNotOrNot
                    , -- Por absurdo, dem de bot asumiendo ~ (~X v ~Y)
                      -- El absurdo se genera en conjunto con ~(X ^ Y)
                      proofBot =
                        PNotE
                            { form = fOrNot
                            , proofNotForm = PAx hNotOrNot
                            , -- ~(X ^ Y), ~(~X v ~Y) |- ~X v ~Y
                              proofForm =
                                POrI1
                                    { proofLeft =
                                        PNotI
                                            { hyp = hypForm x
                                            , -- ~(X ^ Y), ~(~X v ~Y), X |- bot
                                              proofBot =
                                                PNotE
                                                    { form = fOrNot
                                                    , proofNotForm = PAx hNotOrNot
                                                    , -- ~(X ^ Y), ~(~X v ~Y), X |- ~X v ~Y
                                                      proofForm =
                                                        POrI2
                                                            { proofRight =
                                                                PNotI
                                                                    { hyp = hypForm y
                                                                    , -- ~(X ^ Y), ~(~X v ~Y), X, Y |- bot
                                                                      proofBot =
                                                                        PNotE
                                                                            { form = FAnd x y
                                                                            , proofNotForm = PAx hNotAnd
                                                                            , proofForm =
                                                                                PAndI
                                                                                    { proofLeft = PAx $ hypForm x
                                                                                    , proofRight = PAx $ hypForm y
                                                                                    }
                                                                            }
                                                                    }
                                                            }
                                                    }
                                            }
                                    }
                            }
                    }
            }
      where
        fOrNot = FOr (FNot x) (FNot y)
        hNotOrNot = hypForm $ FNot fOrNot

    -- ~X v ~Y |- ~(X ^ Y)
    proofNotDistOverAndRL =
        PNotI
            { hyp = hypForm (FAnd x y)
            , proofBot =
                POrE
                    { left = FNot x
                    , right = FNot y
                    , proofOr = PAx hOr
                    , hypLeft = hypForm $ FNot x
                    , proofAssumingLeft =
                        PNotE
                            { form = x
                            , proofNotForm = PAx (hypForm $ FNot x)
                            , proofForm =
                                PAndE1
                                    { right = y
                                    , proofAnd = PAx (hypForm (FAnd x y))
                                    }
                            }
                    , hypRight = hypForm $ FNot y
                    , proofAssumingRight =
                        PNotE
                            { form = y
                            , proofNotForm = PAx (hypForm $ FNot y)
                            , proofForm =
                                PAndE2
                                    { left = x
                                    , proofAnd = PAx (hypForm (FAnd x y))
                                    }
                            }
                    }
            }

-- Da una demostración para X => Y -|- ~X v Y
proofImpElim :: Form -> Form -> HypId -> HypId -> (Proof, Proof)
proofImpElim x y hImp hOr =
    ( PNamed "imp elim LR" proofImpElim
    , PNamed "imp elim RL" proofOrToImp
    )
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

-- Da una dem de ~~x -|- x
proofDNegElim :: Form -> HypId -> HypId -> (Proof, Proof)
proofDNegElim x hX hDNegX =
    ( PNamed "dneg elim" proofDNegE
    , PNamed "dneg introduce" proofDNegI
    )
  where
    -- x |- ~~x
    proofDNegI =
        PNotI
            { hyp = hNotX
            , -- x, ~x contraducción
              proofBot =
                PNotE
                    { form = x
                    , proofNotForm = PAx hNotX
                    , proofForm = PAx hX
                    }
            }
      where
        hNotX = hypForm $ FNot x
    -- ~~x |- x
    proofDNegE =
        PImpE
            { antecedent = dneg x
            , proofImp = doubleNegElim x
            , proofAntecedent = PAx hDNegX
            }

{- Demuestra la congruencia del ^ sobre el primer argumento, es decir da una
demostración de x ^ y -|- x' ^ y usando que x -|- x'
-}
proofAndCongruence1 ::
    Form ->
    Form ->
    Form ->
    HypId ->
    HypId ->
    HypId ->
    Proof ->
    HypId ->
    Proof ->
    (Proof, Proof)
proofAndCongruence1 x y x' hAnd hAnd' hX proofXThenX' hX' proofX'ThenX =
    ( PNamed "and cong1 LR" proofLR
    , PNamed "and cong1 RL" proofRL
    )
  where
    -- Son simétricas
    proofLR = proofAndCongruence1' x y hAnd hX proofXThenX'
    proofRL = proofAndCongruence1' x' y hAnd' hX' proofX'ThenX

{- Devuelve una demostración de x ^ y |- x' ^ y usando que x |- x'

        x |- x'
    ---------------
    x ^ y |- x' ^ y

mediante cut,

    x ^ y |- x   x ^ y, x |- x' ^ y
    --------------------------- (cut)
    x ^ y |- x' ^ y
-}
proofAndCongruence1' :: Form -> Form -> HypId -> HypId -> Proof -> Proof
proofAndCongruence1' x y hAnd hX proofXThenX' =
    cut x proofX hX proofXThenX'AndY
  where
    proofX =
        PAndE1
            { right = y
            , proofAnd = PAx hAnd
            }
    proofXThenX'AndY =
        PAndI
            { -- x'
              proofLeft = proofXThenX'
            , -- y
              proofRight =
                PAndE2
                    { left = x
                    , proofAnd = PAx hAnd
                    }
            }

{- Demuestra la congruencia del ^ sobre el segundo argumento, es decir da una
demostración de x ^ y -|- x ^ y' usando que y -|- y'
-}
proofAndCongruence2 ::
    Form ->
    Form ->
    Form ->
    HypId ->
    HypId ->
    HypId ->
    Proof ->
    HypId ->
    Proof ->
    (Proof, Proof)
proofAndCongruence2 x y y' hAnd hAnd' hY proofYThenY' hY' proofY'ThenY =
    ( PNamed "and cong2 LR" proofLR
    , PNamed "and cong2 RL" proofRL
    )
  where
    -- Son simétricas
    proofLR = proofAndCongruence2' x y hAnd hY proofYThenY'
    proofRL = proofAndCongruence2' x y' hAnd' hY' proofY'ThenY

-- Devuelve una demostración de x ^ y |- x ^ y' usando que y |- y'
proofAndCongruence2' :: Form -> Form -> HypId -> HypId -> Proof -> Proof
proofAndCongruence2' x y hAnd hY proofYThenY' =
    cut y proofY hY proofYThenXAndY'
  where
    proofY =
        PAndE2
            { left = x
            , proofAnd = PAx hAnd
            }
    proofYThenXAndY' =
        PAndI
            { -- x
              proofLeft =
                PAndE1
                    { right = y
                    , proofAnd = PAx hAnd
                    }
            , -- y'
              proofRight = proofYThenY'
            }

{- Demuestra la congruencia del v sobre el primer argumento, es decir da una
demostración de x v y -|- x' v y usando que x -|- x'
-}
proofOrCongruence1 ::
    Form ->
    Form ->
    Form ->
    HypId ->
    HypId ->
    HypId ->
    Proof ->
    HypId ->
    Proof ->
    (Proof, Proof)
proofOrCongruence1 x y x' hOr hOr' hX proofXThenX' hX' proofX'ThenX =
    ( PNamed "or cong1 LR" proofLR
    , PNamed "or cong1 RL" proofRL
    )
  where
    -- Son simétricas
    proofLR = proofOrCongruence1' x y hOr hX proofXThenX'
    proofRL = proofOrCongruence1' x' y hOr' hX' proofX'ThenX

{- Devuelve una demostración de x v y |- x' v y usando que x |- x'

        x |- x'
    ---------------
    x v y |- x' v y

Dependiendo de qué vale, si x o y, tenemos que introducir el O por x' (mediante
x) o por y (trivial)
-}
proofOrCongruence1' :: Form -> Form -> HypId -> HypId -> Proof -> Proof
proofOrCongruence1' x y hOr hX proofXThenX' =
    POrE
        { left = x
        , right = y
        , proofOr = PAx hOr
        , hypLeft = hX
        , proofAssumingLeft = POrI1{proofLeft = proofXThenX'}
        , hypRight = hY
        , proofAssumingRight = POrI2{proofRight = PAx hY}
        }
  where
    hY = hypForm y

{- Demuestra la congruencia del v sobre el segundo argumento, es decir da una
demostración de x v y -|- x v y' usando que y -|- y'
-}
proofOrCongruence2 ::
    Form ->
    Form ->
    Form ->
    HypId ->
    HypId ->
    HypId ->
    Proof ->
    HypId ->
    Proof ->
    (Proof, Proof)
proofOrCongruence2 x y y' hOr hOr' hY proofYThenY' hY' proofY'ThenY =
    ( PNamed "or cong2 LR" proofLR
    , PNamed "or cong2 RL" proofRL
    )
  where
    -- Son simétricas
    proofLR = proofOrCongruence2' x y hOr hY proofYThenY'
    proofRL = proofOrCongruence2' x y' hOr' hY' proofY'ThenY

-- Devuelve una demostración de x v y |- x v y' usando que y |- y'
proofOrCongruence2' :: Form -> Form -> HypId -> HypId -> Proof -> Proof
proofOrCongruence2' x y hOr hY proofYThenY' =
    POrE
        { left = x
        , right = y
        , proofOr = PAx hOr
        , hypLeft = hX
        , proofAssumingLeft = POrI1{proofLeft = PAx hX}
        , hypRight = hY
        , proofAssumingRight = POrI2{proofRight = proofYThenY'}
        }
  where
    hX = hypForm x

-- Dada una demostración de x' -|- x da una de ~x -|- ~x'
-- (inverso)
proofNotCongruence ::
    Form ->
    Form ->
    HypId ->
    HypId ->
    HypId ->
    Proof ->
    HypId ->
    Proof ->
    (Proof, Proof)
proofNotCongruence x x' hNotX hNotX' hX proofXThenX' hX' proofX'ThenX =
    ( PNamed "not cong LR" proofLR
    , PNamed "not cong RL" proofRL
    )
  where
    proofLR = proofNotCongruence' x x' hNotX hX' proofX'ThenX
    proofRL = proofNotCongruence' x' x hNotX' hX proofXThenX'

-- Dada una demostración de x' |- x da una de ~x |- ~x' (inverso)
proofNotCongruence' ::
    Form ->
    Form ->
    HypId ->
    HypId ->
    Proof ->
    Proof
proofNotCongruence' x x' hNotX hX' proofX'ThenX =
    PNotI
        { hyp = hX'
        , proofBot =
            PNotE
                { form = x
                , proofNotForm = PAx hNotX
                , proofForm = proofX'ThenX
                }
        }

-- TODO: No se necesita porque las imps se eliminan a nots
{- Demuestra la congruencia del => sobre el primer argumento, es decir da una
demostración de x => y -|- x' => y usando que x' -|- x (contravariante)
-}
proofImpCongruence1 ::
    Form ->
    Form ->
    Form ->
    HypId ->
    HypId ->
    HypId ->
    Proof ->
    HypId ->
    Proof ->
    (Proof, Proof)
proofImpCongruence1 x y x' hImp hImp' hX proofXThenX' hX' proofX'ThenX =
    (proofLR, proofRL)
  where
    proofLR = proofImpCongruence1' x y hImp hX' proofX'ThenX
    proofRL = proofImpCongruence1' x' y hImp' hX proofXThenX'

{- Devuelve una demostración de x => y |- x' => y usando que x' |- x

        x' |- x
    ---------------
    x => y |- x' => y
-}
proofImpCongruence1' :: Form -> Form -> HypId -> HypId -> Proof -> Proof
proofImpCongruence1' x y hImp hX' proofX'ThenX =
    PImpI
        { hypAntecedent = hX'
        , -- x', x => y |- y
          proofConsequent =
            PImpE
                { antecedent = x
                , proofImp = PAx hImp
                , proofAntecedent = proofX'ThenX
                }
        }

{- Demuestra la congruencia del => sobre el segundo argumento, es decir da una
demostración de x => y -|- x => y' usando que y -|- y' (covariante)
-}
proofImpCongruence2 ::
    Form ->
    Form ->
    Form ->
    HypId ->
    HypId ->
    HypId ->
    Proof ->
    HypId ->
    Proof ->
    (Proof, Proof)
proofImpCongruence2 x y y' hImp hImp' hY proofYThenY' hY' proofY'ThenY =
    (proofLR, proofRL)
  where
    -- Son simétricas
    proofLR = proofImpCongruence2' x y hImp hY proofYThenY'
    proofRL = proofImpCongruence2' x y' hImp' hY' proofY'ThenY

-- Devuelve una demostración de x => y |- x => y' usando que y |- y'
proofImpCongruence2' :: Form -> Form -> HypId -> HypId -> Proof -> Proof
proofImpCongruence2' x y hImp hY proofYThenY' =
    PImpI
        { hypAntecedent = hX
        , -- x, x => y |- y'
          proofConsequent = cut y pY hY proofYThenY'
        }
  where
    hX = hypForm x
    pY = PImpE{antecedent = x, proofImp = PAx hImp, proofAntecedent = PAx hX}

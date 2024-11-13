module Extractor.Types (
    R,
    FormPi02,
    fromPi02,
    splitForalls,
    toPi02,
) where

import ND.ND (Form (FExists, FForall), VarId)
import Result (Result)

import Text.Printf (printf)

-- Para mas declaratividad en los tipos
type R = Form

-- ([y_1, ..., y_n], exists x f) --> forall y_1 ... forall y_n . exists x. f
type FormPi02 = ([VarId], Form)

-- Interpreta una fórmula como PI^0_2, i.e
--   forall y_1 . forall y_2 ... forall y_n . exists x . f
toPi02 :: Form -> Result FormPi02
toPi02 f = case splitForalls f of
    r@(_, FExists{}) -> return r
    (_, f') -> Left $ printf "pi02: %s is not exists or forall" (show f')

-- forall y_1 . forall y_2 ... forall y_n . f -> ([y_1, ..., y_n], f)
splitForalls :: Form -> FormPi02
splitForalls (FForall y f) =
    let (xs, f') = splitForalls f
     in (y : xs, f')
splitForalls f = ([], f)

fromPi02 :: FormPi02 -> Form
fromPi02 (vs, f) = foldr FForall f vs

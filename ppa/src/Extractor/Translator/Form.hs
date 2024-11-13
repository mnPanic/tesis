module Extractor.Translator.Form (translateF) where

import Extractor.Types (R)
import ND.ND (Form (..))

-- Traduce f via doble negación relativizada, parametrizada por una fórmula
-- arbitraria R.
-- FNot a ~~> FImp a r (FNot_R)
translateF :: Form -> R -> Form
translateF form r = case form of
  FAnd left right -> FAnd (rec left) (rec right)
  FOr left right -> fNotR (FAnd (fNotR (rec left)) (fNotR (rec right)))
  FImp left right -> FImp (rec left) (rec right)
  FNot g -> fNotR (rec g)
  FForall x g -> FForall x (rec g)
  FExists x g -> fNotR (FForall x (fNotR (rec g)))
  FFalse -> r
  FTrue -> FTrue
  g@(FPred{}) -> fNotR (fNotR g)
 where
  rec form' = translateF form' r
  fNotR form' = FImp form' r
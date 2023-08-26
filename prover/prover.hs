module Main where

-- Tipos de identificadores
type VarId String
type FunId String
type PredId String
type HypId String

data Term =
    TVar VarId
    | TFun FunId [Term]

data Form =
    FPred PredId [Term]
    | FAnd Form Form
    | FOr Form Form
    | FImp Form Form
    | FNot Form
    | FTrue
    | FFalse
    | FForall VarId Form
    | FExists VarId Form

data Proof =
    PAx HypId
    | PAndI Proof Proof -- <t, s>
    | PAndE1 Form Proof 
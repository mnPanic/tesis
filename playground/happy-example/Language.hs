module Language where

data EExp
    = Let String EExp EExp
    | Exp1 EExp1
    deriving (Show)

data EExp1
    = Plus EExp1 ETerm
    | Minus EExp1 ETerm
    | Term ETerm
    deriving (Show)

data ETerm
    = Times ETerm EFactor
    | Div ETerm EFactor
    | Factor EFactor
    deriving (Show)

data EFactor
    = Int Int
    | Var String
    | Brack EExp
    deriving (Show)
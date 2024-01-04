{
module Parser(parseExp) where

import Prover ( Form(..), Term(..) )
import Lexer ( Token(..) )
}

%name parseExp
%tokentype { Token }
%error { parseError }

%token
    '('     { TokenOB }
    ')'     { TokenCB }
    and     { TokenAnd }
    or      { TokenOr }
    imp     { TokenImp }
    not     { TokenNot }
    true    { TokenTrue }
    false   { TokenFalse }
    forall  { TokenForall }
    exists  { TokenExists }
    dot     { TokenDot }

    id      { TokenId $$ }
    var     { TokenVar $$ }

%right exists forall dot
%right imp
%left and or
%nonassoc not
%%

Exp     : Form                      { $1 }

Form    : id '(' Terms ')'          { FPred $1 $3 }
        | Form and Form             { FAnd $1 $3 }
        | Form or Form              { FOr $1 $3 }
        | Form imp Form             { FImp $1 $3 }
        | not Form                  { FNot $2 }
        | exists var dot Form       { FExists $2 $4 }
        | forall var dot Form       { FForall $2 $4 }
        | true                      { FTrue }
        | false                     { FFalse }

Term    : var                       { TVar $1 }
        | id '(' Terms ')'          { TFun $1 $3 }

Terms   : {- empty -}               { [] }
        | Term Terms                { $1 : $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}

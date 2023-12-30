{
module Parser(calc) where

import Language
import Lexer
}

%name calc
%tokentype { Token }
%error { parseError }

%token
    let     { TokenLet }
    in      { TokenIn }
    int     { TokenInt $$ }
    var     { TokenVar $$ }
    '='     { TokenEq }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    '*'     { TokenTimes }
    '/'     { TokenDiv }
    '('     { TokenOB }
    ')'     { TokenCB }

%%

Exp     : let var '=' Exp in Exp    { Let $2 $4 $6}
        | Exp1                      { Exp1 $1 }

Exp1    : Exp1 '+' Term             { Plus $1 $3 }
        | Exp1 '-' Term             { Minus $1 $3 }
        | Term                      { Term $1 }

Term    : Term '*' Factor           { Times $1 $3 }
        | Term '/' Factor           { Div $1 $3 }
        | Factor                    { Factor $1 }

Factor  : int                       { Int $1 }
        | var                       { Var $1 }
        | '(' Exp ')'               { Brack $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
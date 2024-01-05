{
module Parser(parseExp) where

import Prover ( Form(..), Term(..) )
import Lexer ( Token(..) )
}

%name parseExp
%tokentype { Token }
%error { parseError }

-- % https://monlih.github.io/happy-docs/#_sec_errorhandlertype_directive
%errorhandlertype explist

%token
    '('         { TokenParenOpen }
    ')'         { TokenParenClose }
    and         { TokenAnd }
    or          { TokenOr }
    imp         { TokenImp }
    not         { TokenNot }
    true        { TokenTrue }
    false       { TokenFalse }
    forall      { TokenForall }
    exists      { TokenExists }
    dot         { TokenDot }
    comma       { TokenComma }

    id          { TokenId $$ }
    var         { TokenVar $$ }

%right exists forall dot
%right imp
%left and or
%nonassoc not
%%

Exp     : Form                      { $1 }

Form :: { Form }
Form    : id TermArgs               { FPred $1 $2 }
        | Form and Form             { FAnd $1 $3 }
        | Form or Form              { FOr $1 $3 }
        | Form imp Form             { FImp $1 $3 }
        | not Form                  { FNot $2 }
        | exists var dot Form       { FExists $2 $4 }
        | forall var dot Form       { FForall $2 $4 }
        | true                      { FTrue }
        | false                     { FFalse }
        | '(' Form ')'              { $2 }

Term :: { Term }
Term    : var                       { TVar $1 }
        | id TermArgs               { TFun $1 $2 }

TermArgs :: { [Term] }
TermArgs : {- empty -}              { [] }
         | '(' Terms ')'            { $2 }

Terms :: { [Term] }
Terms   : Term                      { [$1] }
        | Term comma Terms          { $1 : $3 }

{
parseError :: ([Token], [String]) -> a
parseError (r, n) = error (
        "Parse error on " ++ show r ++ "\npossible tokens: " ++ show n)
}

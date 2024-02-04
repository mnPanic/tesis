{
module Parser(parseExp) where

import Prover ( Form(..), Term(..) )
import Theory ( Proof, ProofStep(..), Theorem(..), Program(..) )
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
    ';'         { TokenSemicolon }
    ':'         { TokenDoubleColon }
    theorem     { TokenTheorem }
    proof       { TokenProof }
    qed         { TokenQED }
    name        { TokenQuotedName $$ }
    assume      { TokenAssume }
    thus        { TokenThus }
    by          { TokenBy }

%right exists forall dot
%right imp
%left and or
%nonassoc not
%%

Prog    :: { Program }
Prog    : Theorem                   { ProgramT $1 }
        | Form                      { ProgramF $1 }

Theorem :: { Theorem }
Theorem : theorem name ':' Form proof Proof qed   { Theorem $2 $4 $6 }

Proof   :: { Proof }
Proof   : ProofStep ';' Proof       { $1 : $3 }
        | ProofStep ';'             { [ $1 ] }

ProofStep       :: { ProofStep }
ProofStep       : assume name ':' Form       { PSAssume $2 $4 }
                | thus Form by name          { PSThus $2 $4 }

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

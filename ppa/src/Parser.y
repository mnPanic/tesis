{
module Parser(parseExp) where

import ND ( Form(..), Term(..) )
import PPA ( TProof, ProofStep(..), Program(..), Decl(..), Justification )
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

    id          { TokenId $$ }
    var         { TokenVar $$ }
    ';'         { TokenSemicolon }
    ':'         { TokenDoubleColon }
    ','         { TokenComma }
    axiom       { TokenAxiom }   
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
Prog    : Declarations                   { $1 }

Declarations :: { [Decl] }
Declarations : Declaration Declarations         { $1 : $2}
             | Declaration                      { [$1] }

Declaration :: { Decl }
Declaration : Axiom                     { $1 }
            | Theorem                   { $1 }

Axiom :: { Decl }
Axiom : axiom name ':' Form             { DAxiom $2 $4 }

Theorem :: { Decl }
Theorem : theorem name ':' Form proof Proof qed   { DTheorem $2 $4 $6 }

Proof   :: { TProof }
Proof   : ProofStep ';' Proof       { $1 : $3 }
        | ProofStep ';'             { [ $1 ] }

ProofStep :: { ProofStep }
ProofStep : assume name ':' Form                { PSAssume $2 $4 }
          | thus Form by Justification          { PSThusBy $2 $4 }

Justification :: { Justification }
Justification : name ',' Justification          { $1 : $3 }
              | name                            { [ $1 ] }              

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
        | Term ',' Terms          { $1 : $3 }

{
parseError :: ([Token], [String]) -> a
parseError (r, n) = error (
        "Parse error on " ++ show r ++ "\npossible tokens: " ++ show n)
}

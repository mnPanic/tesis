{
module Parser(parseProgram, parseProgram', parseTerm) where

import ND ( Form(..), Term(..) )
import PPA ( TProof, ProofStep(..), Program(..), Decl(..), Justification, Case )
import Lexer
import Data.List (intercalate)
import Debug.Trace (trace)
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { parseError }

-- https://monlih.github.io/happy-docs/#_sec_multiple_parsers
%name hParseProgram Prog
%name hParseTerm Term

-- % https://monlih.github.io/happy-docs/#_sec_errorhandlertype_directive
%errorhandlertype explist

%token
    '('         { Token _ TokenParenOpen }
    ')'         { Token _ TokenParenClose }
    and         { Token _ TokenAnd }
    or          { Token _ TokenOr }
    imp         { Token _ TokenImp }
    not         { Token _ TokenNot }
    true        { Token _ TokenTrue }
    false       { Token _ TokenFalse }
    forall      { Token _ TokenForall }
    exists      { Token _ TokenExists }
    dot         { Token _ TokenDot }

    id                  { Token _ (TokenId $$) }
    var                 { Token _ (TokenVar $$) }
    ':'                 { Token _ TokenDoubleColon }
    ','                 { Token _ TokenComma }
    axiom               { Token _ TokenAxiom }   
    theorem             { Token _ TokenTheorem }
    proof               { Token _ TokenProof }
    end                 { Token _ TokenEnd }
    name                { Token _ (TokenQuotedName $$) }
    suppose             { Token _ TokenSuppose }
    thus                { Token _ TokenThus }
    then                { Token _ TokenThen }
    hence               { Token _ TokenHence }
    have                { Token _ TokenHave }
    by                  { Token _ TokenBy }
    equivalently        { Token _ TokenEquivalently }
    claim               { Token _ TokenClaim }
    case                { Token _ TokenCase }
    cases               { Token _ TokenCases }
    take                { Token _ TokenTake }
    ':='                { Token _ TokenAssign }
    consider            { Token _ TokenConsider }
    st                  { Token _ TokenSuchThat }
    let                 { Token _ TokenLet }       

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
Axiom : axiom Name ':' Form             { DAxiom $2 $4 }

Theorem :: { Decl }
Theorem : theorem Name ':' Form proof Proof end   { DTheorem $2 $4 $6 }

Proof   :: { TProof }
Proof   : ProofStep Proof       { $1 : $2 }
        | {- empty -}           { [] }

ProofStep :: { ProofStep }
ProofStep : suppose Name ':' Form                              { PSSuppose $2 $4 }
          | thus Form OptionalBy                               { PSThusBy $2 $3 }
          | hence Form OptionalBy                              { PSThusBy $2 (["-"] ++ $3) }
          | have Name ':' Form OptionalBy                      { PSHaveBy $2 $4 $5 }
          | then Name ':' Form OptionalBy                      { PSHaveBy $2 $4 (["-"] ++ $5) }
          | equivalently Form                                  { PSEquiv $2 }
          | claim Name ':' Form proof Proof end                { PSClaim $2 $4 $6 }
          | cases OptionalBy Cases end                         { PSCases $2 $3 }
          | take var ':=' Term                                 { PSTake $2 $4 }
          | let var                                            { PSLet $2 }
          | consider var st Name ':' Form by Justification     { PSConsider $2 $4 $6 $8 }

Cases   :: { [Case] }
Cases   : Case Cases      { $1 : $2 }
        | {- empty -}     { [] }

Case    :: { Case }
Case    : case Form Proof               { ("-", $2, $3) }
        | case Name ':' Form Proof      { ($2, $4, $5) }

OptionalBy :: { Justification }
OptionalBy : by Justification      { $2 }
OptionalBy : {- empty -}           { [] }

Justification :: { Justification }
Justification : Name ',' Justification          { $1 : $3 }
              | Name                            { [ $1 ] }

Name    :: { String }
Name    : id               { $1 }
        | name             { $1 }

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
        | Term ',' Terms            { $1 : $3 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

parseError :: (Token, [String]) -> Alex a
parseError ((Token p t), next) =
        alexError' p ("parse error at token '" ++ unLex t ++ "', possible tokens:")
        --alexError' p ("parse error at token '" ++ unLex t ++ "', possible tokens:" ++ trace ("a" ++ (show $ null next)) (show next))

parseProgram' :: FilePath -> String -> Either String Program
parseProgram' = runAlex' hParseProgram

parseProgram :: String -> Either String Program
parseProgram s = runAlex s hParseProgram

parseTerm :: String -> Either String Term
parseTerm s = runAlex s hParseTerm
}

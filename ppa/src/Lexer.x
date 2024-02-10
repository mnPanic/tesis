{
module Lexer(lexer, Token(..), AlexPosn(..)) where
}

-- https://github.com/haskell/alex/blob/master/examples/Tokens_posn.x
%wrapper "posn"

-- Actions have type AlexPosn -> String -> Token
tokens :-
    $white+         ;
    "//".*          ;
    \.              { \_ _ -> TokenDot }
    \,              { \_ _ -> TokenComma }
    \&|\^           { \_ _ -> TokenAnd }
    \||v            { \_ _ -> TokenOr }
    true            { \_ _ -> TokenTrue }
    false           { \_ _ -> TokenFalse }
    =>              { \_ _ -> TokenImp }
    (\¬|\~)         { \_ _ -> TokenNot }
    exists          { \_ _ -> TokenExists }
    forall          { \_ _ -> TokenForall }
    \(              { \_ _ -> TokenParenOpen }
    \)              { \_ _ -> TokenParenClose }
    theorem         { \_ _ -> TokenTheorem }
    proof           { \_ _ -> TokenProof }
    qed             { \_ _ -> TokenQED }
    \;              { \_ _ -> TokenSemicolon }
    \:              { \_ _ -> TokenDoubleColon }
    assume          { \_ _ -> TokenAssume }
    thus            { \_ _ -> TokenThus }
    by              { \_ _ -> TokenBy }

    \".*\"          { const TokenQuotedName }

    (\_|[A-Z])[a-zA-Z0-9\_\-]*(\')*                      { const TokenVar }
    [a-zA-Z0-9\_\-\?!#\$\%&\*\+\<\>\=\?\@\^]+(\')*       { const TokenId }
    
{
data Token 
    -- Fórmulas
    = TokenId String
    | TokenVar String
    | TokenAnd
    | TokenOr
    | TokenNot
    | TokenTrue
    | TokenFalse
    | TokenImp
    | TokenExists
    | TokenForall
    | TokenDot
    | TokenComma
    | TokenParenOpen
    | TokenParenClose
    -- Teoremas
    | TokenSemicolon
    | TokenDoubleColon
    | TokenTheorem
    | TokenProof
    | TokenQED
    | TokenQuotedName String
    | TokenAssume
    | TokenThus
    | TokenBy
    deriving (Eq, Show)

lexer = alexScanTokens
}
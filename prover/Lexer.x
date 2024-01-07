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

    (\_|[A-Z])[a-zA-Z0-9\_\-]*(\')*                        { const TokenVar }
    [a-zA-Z0-9\_\-\?!#\$\%&\*\+\:\;\<\>\=\?\@\^]+(\')*     { const TokenId }
    
{
data Token 
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
    deriving (Eq, Show)

lexer = alexScanTokens
}
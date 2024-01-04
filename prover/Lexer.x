{
module Lexer(lexer, Token(..)) where
}

%wrapper "basic"

-- Actions have type String -> Token
tokens :-
    $white+         ;
    "//".*          ;
    \&|\^           { const TokenAnd }
    \||v            { const TokenOr }
    true            { const TokenTrue }
    false           { const TokenFalse }
    =>              { const TokenImp }
    (\!|\~)         { const TokenNot }
    exists          { const TokenExists }
    forall          { const TokenForall }
    \.              { const TokenDot }
    \(              { const TokenOB }
    \)              { const TokenCB }

    (\_|[A-Z])[a-zA-Z0-9\_\-]*(\')*                        { TokenVar }
    [a-zA-Z0-9\_\-\?!#\$\%&\*\+\:\;\<\>\=\?\@\^]+(\')*     { TokenId }
    
{
data Token 
    = TokenId String
    | TokenVar String
    | TokenAnd
    | TokenOr
    | TokenDot
    | TokenNot
    | TokenTrue
    | TokenFalse
    | TokenImp
    | TokenExists
    | TokenForall
    | TokenOB
    | TokenCB
    deriving (Eq, Show)

lexer = alexScanTokens
}
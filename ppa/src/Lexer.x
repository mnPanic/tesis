{
module Lexer(lexer, Token(..), AlexPosn(..)) where
}

-- https://github.com/haskell/alex/blob/master/examples/Tokens_posn.x
%wrapper "posn"

-- Actions have type AlexPosn -> String -> Token
tokens :-
    $white+         ;
    "//".*          ;
    \.              { literal TokenDot }
    \,              { literal TokenComma }
    \&|\^           { literal TokenAnd }
    \||v            { literal TokenOr }
    true            { literal TokenTrue }
    false           { literal TokenFalse }
    =>              { literal TokenImp }
    (\¬|\~)         { literal TokenNot }
    exists          { literal TokenExists }
    forall          { literal TokenForall }
    \(              { literal TokenParenOpen }
    \)              { literal TokenParenClose }
    axiom           { literal TokenAxiom }
    theorem         { literal TokenTheorem }
    proof           { literal TokenProof }
    qed             { literal TokenQED }
    \;              { literal TokenSemicolon }
    \:              { literal TokenDoubleColon }
    assume          { literal TokenAssume }
    thus            { literal TokenThus }
    by              { literal TokenBy }

    \".*\"          { \_ n -> TokenQuotedName (firstLast n)}

    (\_|[A-Z])[a-zA-Z0-9\_\-]*(\')*                      { const TokenVar }
    [a-zA-Z0-9\_\-\?!#\$\%&\*\+\<\>\=\?\@\^]+(\')*       { const TokenId }
    
{
firstLast :: [a] -> [a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

literal :: Token -> (AlexPosn -> String -> Token)
literal t _ _ = t

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
    | TokenAxiom
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
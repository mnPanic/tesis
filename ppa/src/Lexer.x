{
module Lexer(
    Token(..)
    , TokenClass(..)
    , unLex
    , AlexPosn(..)
    , Alex(..)
    , runAlex'
    , runAlex
    , alexMonadScan'
    , alexError'
    , alexInitUserState
    ) where

import Prelude hiding (lex)
import Control.Monad ( liftM )
}

-- https://github.com/dagit/happy-plus-alex/tree/master
-- https://github.com/haskell/alex/blob/master/examples/Tokens_posn.x
%wrapper "monadUserState"

-- Actions have type AlexPosn -> String -> Token
tokens :-
    $white+         ;
    "//".*          ;
    \.              { literal TokenDot }
    \,              { literal TokenComma }
    \&              { literal TokenAnd }
    \|              { literal TokenOr }
    true            { literal TokenTrue }
    false           { literal TokenFalse }
    \-\>            { literal TokenImp }
    \~              { literal TokenNot }
    exists          { literal TokenExists }
    forall          { literal TokenForall }
    \(              { literal TokenParenOpen }
    \)              { literal TokenParenClose }
    axiom           { literal TokenAxiom }
    theorem         { literal TokenTheorem }
    proof           { literal TokenProof }
    end             { literal TokenEnd }
    \;              { literal TokenSemicolon }
    \:              { literal TokenDoubleColon }
    suppose         { literal TokenSuppose }
    thus            { literal TokenThus }
    hence           { literal TokenHence }
    have            { literal TokenHave }
    then            { literal TokenThen }
    by              { literal TokenBy }

    \"[^\"]*\"          { lex (TokenQuotedName . firstLast) }

    (\_|[A-Z])[a-zA-Z0-9\_\-]*(\')*                      { lex TokenVar }
    [a-zA-Z0-9\_\-\?!#\$\%&\*\+\<\>\=\?\@\^]+(\')*       { lex TokenId }
    
{

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Eq, Show )

data TokenClass 
    = TokenEOF
    -- Fórmulas
    | TokenId String
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
    -- Programas
    | TokenAxiom
    | TokenSemicolon
    | TokenDoubleColon
    | TokenTheorem
    | TokenProof
    | TokenEnd
    | TokenQuotedName String
    | TokenSuppose
    | TokenThus
    | TokenThen
    | TokenHence
    | TokenHave
    | TokenBy
    deriving (Eq, Show)

unLex :: TokenClass -> String
unLex TokenDot = "."
unLex TokenComma = ","
unLex TokenAnd = "&"
unLex TokenOr = "|"
unLex TokenTrue = "true"
unLex TokenFalse = "false"
unLex TokenImp = "->"
unLex TokenNot = "~"
unLex TokenExists = "exists"
unLex TokenForall = "forall"
unLex TokenParenOpen = "("
unLex TokenParenClose = ")"
unLex TokenAxiom = "axiom"
unLex TokenTheorem = "theorem"
unLex TokenProof = "proof"
unLex TokenEnd = "qed"
unLex TokenSemicolon = ";"
unLex TokenDoubleColon = ":"
unLex TokenSuppose = "suppose"
unLex TokenThus = "thus"
unLex TokenBy = "by"
unLex TokenThen = "then"
unLex TokenHence = "hence"
unLex TokenHave = "have"
unLex (TokenQuotedName n) = "\"" ++ n ++ "\""
unLex (TokenVar s) = "(var) " ++ s
unLex (TokenId s) = "(id) " ++ s

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

firstLast :: [a] -> [a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
literal :: TokenClass -> AlexAction Token
literal = lex . const


-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
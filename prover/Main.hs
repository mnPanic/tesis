module Main where

import Lexer (lexer)
import Parser (parseExp)
import Theory (Program, execute)

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    raw <- case args of
        [] -> getContents
        [f] -> readFile f
        _ -> error "expected max. 1 argument "
    print $ execute $ parse raw

parse :: String -> Program
parse = parseExp . lexer
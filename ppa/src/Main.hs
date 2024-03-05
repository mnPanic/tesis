module Main where

import Certifier (certify)
import Lexer (lexer)
import PPA (Program)
import Parser (parseExp)

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    raw <- case args of
        [] -> getContents
        [f] -> readFile f
        _ -> error "expected max. 1 argument "
    print $ certify $ parse raw
    --print $ lexer raw

parse :: String -> Program
parse = parseExp . lexer
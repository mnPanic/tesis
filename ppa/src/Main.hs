module Main where

import Certifier (certify)
import Parser (parseProgram)

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let path = case args of
            [] -> "<stdin>"
            [f] -> f
            _ -> error "expected max. 1 argument "

    raw <- case path of
        "<stdin>" -> getContents
        f -> readFile f

    let result = parseProgram path raw
    either putStrLn (print . certify) result
module Main where

import Certifier (certify, checkContext)
import Parser (parseProgram')

import GHC.Stack (HasCallStack)
import NDProofs (Result)
import PPA (Program)
import System.Environment (getArgs)

main :: (HasCallStack) => IO ()
main = do
    args <- getArgs
    let path = case args of
            [] -> "<stdin>"
            [f] -> f
            _ -> error "expected max. 1 argument "

    raw <- case path of
        "<stdin>" -> getContents
        f -> readFile f

    let result = parseProgram' path raw
    case result of
        Left err -> putStrLn err
        Right prog -> case execute prog of
            Left err -> putStrLn err
            Right _ -> putStrLn "OK!"

execute :: Program -> Result ()
execute prog = do
    ctx <- certify prog
    checkContext ctx
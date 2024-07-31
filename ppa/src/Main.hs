module Main where

import Certifier (certify, checkContext, reduceContext)
import Parser (parseProgram')

import Data.Text.Lazy (unpack)
import GHC.Stack (HasCallStack)
import NDProofs (Result)
import PPA (Context, Program)
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint, pShowNoColor)
import Text.Printf (printf)

data Args = Args {input :: Path, output :: Maybe Path}

data Path = Stdin | Stdout | File FilePath

instance Show Path where
    show Stdin = "<stdin>"
    show Stdout = "<stdout>"
    show (File p) = p

main :: (HasCallStack) => IO ()
main = do
    rawArgs <- getArgs
    let args = parseArgs rawArgs

    let inputPath = input args
    rawProgram <- case inputPath of
        Stdin -> getContents
        File f -> readFile f

    putStrLn "Running program..."
    case run (show inputPath) rawProgram of
        Left err -> putStrLn err
        Right ctx -> do
            putStrLn "OK!"
            writeResult (output args) ctx

writeResult :: Maybe Path -> Context -> IO ()
writeResult Nothing _ = return ()
writeResult (Just p) ctx = do
    putStrLn "Reducing..."
    let ctxReduced = reduceContext ctx
    case checkContext ctxReduced of
        Right () -> putStrLn "OK!"
        Left err -> putStrLn err

    case p of
        Stdout -> do
            putStrLn "raw context:\n"
            pPrint ctx
            putStrLn "reduced:\n"
            pPrint ctxReduced
        File f -> do
            writeFile (f ++ "_raw.nk") (unpack $ pShowNoColor ctx)
            writeFile (f ++ "_red.nk") (unpack $ pShowNoColor ctxReduced)

run :: String -> String -> Result Context
run path rawProgram = do
    prog <- parseProgram' (show path) rawProgram
    ctx <- certify prog
    checkContext ctx
    return ctx

parseArgs :: [String] -> Args
parseArgs args = case args of
    [] -> Args{input = Stdin, output = Nothing}
    [f] -> Args{input = inputPath f, output = Nothing}
    [f, o] -> Args{input = inputPath f, output = Just $ outputPath o}
  where
    inputPath s = if s == "-" then Stdin else File s
    outputPath s = if s == "-" then Stdout else File s
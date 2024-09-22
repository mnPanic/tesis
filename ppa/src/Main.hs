{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Certifier (certify, checkContext, reduceContext, translateContext)
import Parser (parseProgram')

import Data.Text.Lazy (unpack)
import GHC.Stack (HasCallStack)
import NDProofs (Result)
import PPA (Context, Program)
import System.Environment (getArgs)

import Certifier (certify, checkContext, reduceContext, translateContext)
import ND (fPred0)
import NDProofs (Result)
import PPA (Context)
import Parser (parseProgram')
import PrettyShow (PrettyShow (prettyShow))
import Text.Pretty.Simple (
    CheckColorTty (NoCheckColorTty),
    OutputOptions (outputOptionsCompact, outputOptionsCompactParens),
    defaultOutputOptionsNoColor,
    pPrint,
    pPrintOpt,
    pShowNoColor,
    pShowOpt,
 )
import Text.Printf (printf)

-- import Text.RawString.QQ

data Args = Args {input :: Path, output :: Maybe Path}

data Path = Stdin | Stdout | File FilePath

instance Show Path where
    show Stdin = "<stdin>"
    show Stdout = "<stdout>"
    show (File p) = p

-- main :: (HasCallStack) => IO ()
-- main =
--     run2
--         [r|
-- axiom ax: b
-- axiom 1: b -> c
-- theorem t: c
-- proof
--     thus c by ax, 1
-- end

{- | ]
         Nothing
-}
main :: (HasCallStack) => IO ()
main = do
    rawArgs <- getArgs
    let args = parseArgs rawArgs

    let inputPath = input args
    rawProgram <- case inputPath of
        Stdin -> getContents
        File f -> readFile f

    run2 (show inputPath) rawProgram (output args)

run2 :: String -> String -> Maybe Path -> IO ()
run2 path rawProgram output = do
    putStr "Running program..."
    case run path rawProgram of
        Left err -> putStrLn err
        Right ctx -> do
            putStrLn "OK!"
            putStr "Translating..."
            let ctxT = translateContext ctx (fPred0 "__r")
            putStrLn "OK!"
            putStr "Checking translated..."
            case checkContext ctxT of
                Left err -> putStrLn err
                Right _ -> do
                    -- r -> do
                    putStrLn "OK!"
                    putStr "Reducing..."
                    let ctxR = reduceContext ctxT
                    putStrLn "OK!"

                    putStr "Checking reduced..."
                    case checkContext ctxR of
                        Left err -> putStrLn err
                        Right _ -> do
                            putStrLn "OK!"
                            writeResult output ctx ctxR

writeResult :: Maybe Path -> Context -> Context -> IO ()
writeResult Nothing _ _ = return ()
writeResult (Just p) ctxOriginal ctxReduced = do
    putStrLn "Writing..."
    case p of
        Stdout -> do
            putStrLn "raw context:\n"
            pPrint ctxOriginal
            putStrLn "reduced:\n"
            pPrint ctxReduced
        File f -> do
            let file_raw = (f ++ "_raw.nk")
            let file_reduced = (f ++ ".nj")
            writeFile file_raw (prettyShow ctxOriginal)
            putStrLn ("Wrote raw to " ++ file_raw)
            writeFile file_reduced (prettyShow ctxReduced)
            putStrLn ("Wrote translated+reduced to " ++ file_reduced)

-- writeFile (f ++ "_red.nk") (prettyShow ctxReduced)

-- case checkContext ctxReduced of
--     Right () -> putStrLn "Reduced OK!"
--     Left err -> putStrLn $ printf "Reduced failed: %s" err

-- prettyShow :: (Show a) => a -> String
-- prettyShow s = unpack $ pShowOpt opts s
--   where
--     opts =
--         defaultOutputOptionsNoColor
--             { outputOptionsCompact = True
--             -- , outputOptionsCompactParens = True
--             }

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
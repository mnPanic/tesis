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
import ND (HypId, fPred0)
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

data Args
    = ArgsCheck {input :: Path, output :: Maybe Path}
    | ArgsTranslate {input :: Path, output :: Maybe Path, theorem :: HypId}

data Path = Std | File FilePath

instance Show Path where
    show Std = "<std>"
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

    case args of
        ArgsCheck{} -> runCheck args
        ArgsTranslate{} -> runTranslate args

runCheck :: Args -> IO ()
runCheck (ArgsCheck inPath outPath) = do
    rawProgram <- case inPath of
        Std -> getContents
        File f -> readFile f

    putStr "Checking..."
    let ctx = parseAndCheck (show inPath) rawProgram
    putStrLn "OK!"

runTranslate :: Args -> IO ()
runTranslate (ArgsTranslate inPath outPath theorem) = do
    rawProgram <- case inPath of
        Std -> getContents
        File f -> readFile f

    putStr "Running program..."
    case parseAndCheck (show inPath) rawProgram of
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
                            writeResult outPath ctx ctxR

writeResult :: Maybe Path -> Context -> Context -> IO ()
writeResult Nothing _ _ = return ()
writeResult (Just p) ctxOriginal ctxReduced = do
    putStrLn "Writing..."
    case p of
        Std -> do
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

parseAndCheck :: String -> String -> Result Context
parseAndCheck path rawProgram = do
    prog <- parseProgram' (show path) rawProgram
    ctx <- certify prog
    checkContext ctx
    return ctx

parseArgs :: [String] -> Args
parseArgs (cmd : args) = case cmd of
    "check" -> parseCheckArgs args
    "translate" -> parseTranslateArgs args

parseTranslateArgs :: [String] -> Args
parseTranslateArgs args = case args of
    [t] -> ArgsTranslate{theorem = t, input = Std, output = Nothing}
    [t, f] -> ArgsTranslate{theorem = t, input = parsePath f, output = Nothing}
    [t, f, o] -> ArgsTranslate{theorem = t, input = parsePath f, output = Just $ parsePath o}

parseCheckArgs :: [String] -> Args
parseCheckArgs args = case args of
    [] -> ArgsCheck{input = Std, output = Nothing}
    [f] -> ArgsCheck{input = parsePath f, output = Nothing}
    [f, o] -> ArgsCheck{input = parsePath f, output = Just $ parsePath o}

parsePath :: String -> Path
parsePath "-" = Std
parsePath s = File s
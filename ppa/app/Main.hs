module Main where

import Args (Args (..), Path (..), parseArgs)
import Extractor.Extractor (extractWitnessCtx)
import PPA.Certifier (certify, checkContext)
import PPA.PPA (Context)
import PPA.Parser (parseProgram', parseTerm)
import PPA.Proofs (Result)
import PrettyShow (PrettyShow (prettyShow))

import System.Environment (getArgs)

import GHC.Stack (HasCallStack)
import Text.Pretty.Simple (
    pPrint,
 )
import Text.Printf (printf)

main :: (HasCallStack) => IO ()
main = do
    rawArgs <- getArgs
    case parseArgs rawArgs of
        Left err -> putStrLn err
        Right args -> case args of
            ArgsCheck{} -> runCheck args
            ArgsExtract{} -> runExtract args

runCheck :: Args -> IO ()
runCheck (ArgsCheck inPath outPath) = do
    rawProgram <- case inPath of
        Std -> getContents
        File f -> readFile f

    putStr "Checking... "
    case parseAndCheck (show inPath) rawProgram of
        Left err -> putStrLn err
        Right ctx -> do
            putStrLn "OK!"
            case outPath of
                Nothing -> return ()
                Just path -> do
                    putStrLn "Writing..."
                    case path of
                        Std -> do
                            putStrLn "context:\n"
                            pPrint ctx
                        File f -> do
                            let file_raw = f ++ "_raw.nk"
                            writeFile file_raw (prettyShow ctx)
                            putStrLn ("Wrote raw to " ++ file_raw)

runExtract :: Args -> IO ()
runExtract (ArgsCheck{}) = undefined
runExtract (ArgsExtract inPath outPath theoremId terms) = do
    rawProgram <- case inPath of
        Std -> getContents
        File f -> readFile f

    case mapM parseTerm terms of
        Left err -> putStrLn $ "Parsing terms: " ++ err
        Right parsedTerms -> do
            putStr "Running program... "
            case parseAndCheck (show inPath) rawProgram of
                Left err -> putStrLn err
                Right ctx -> do
                    putStrLn "OK!"
                    putStr "Translating... "
                    case extractWitnessCtx ctx theoremId parsedTerms of
                        Left err -> putStrLn err
                        Right (ctx', t, f) -> do
                            putStrLn "OK!"
                            writeResult outPath ctx ctx'
                            putStr "Checking translated... "
                            case checkContext ctx' of
                                Left err -> putStrLn err
                                Right _ -> do
                                    putStrLn "OK!"
                                    putStrLn $ printf "Extracted witness: %s" (show t)
                                    putStrLn $ printf "of formula: %s" (show f)

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
            let file_raw = f ++ "_raw.nk"
            writeFile file_raw (prettyShow ctxOriginal)
            putStrLn ("Wrote raw to " ++ file_raw)

            let file_reduced = f ++ ".nj"
            writeFile file_reduced (prettyShow ctxReduced)
            putStrLn ("Wrote translated+reduced to " ++ file_reduced)

parseAndCheck :: String -> String -> Result Context
parseAndCheck path rawProgram = do
    prog <- parseProgram' path rawProgram
    ctx <- certify prog
    checkContext ctx
    return ctx

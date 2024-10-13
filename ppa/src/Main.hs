module Main where

import Certifier (certify, checkContext)
import NDExtractor (extractWitnessCtx)

import GHC.Stack (HasCallStack)
import ND (HypId)
import NDProofs (Result)
import PPA (Context)
import System.Environment (getArgs)

import Parser (parseProgram', parseTerm)
import PrettyShow (PrettyShow (prettyShow))
import Text.Pretty.Simple (
    pPrint,
 )
import Text.Printf (printf)

-- import Text.RawString.QQ

data Args
    = ArgsCheck {input :: Path, output :: Maybe Path}
    | ArgsExtract
        { input :: Path
        , output :: Maybe Path
        , theorem :: HypId
        , terms :: [String]
        , skipCheck :: Bool
        }

data Path = Std | File FilePath

instance Show Path where
    show Std = "<std>"
    show (File p) = p

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
runExtract (ArgsExtract inPath outPath theoremId terms skipCheck) = do
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
                        Right (ctx', t) -> do
                            putStrLn "OK!"
                            if skipCheck
                                then do
                                    putStrLn "Skipping check"
                                    putStrLn $ printf "Extracted witness: %s" (show t)
                                    writeResult outPath ctx ctx'
                                else do
                                    putStr "Checking translated... "
                                    case checkContext ctx' of
                                        Left err -> putStrLn err
                                        Right _ -> do
                                            putStrLn "OK!"
                                            putStrLn $ printf "Extracted witness: %s" (show t)
                                            writeResult outPath ctx ctx'

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
    prog <- parseProgram' path rawProgram
    ctx <- certify prog
    checkContext ctx
    return ctx

parseArgs :: [String] -> Result Args
parseArgs [] = Left "empty args"
parseArgs (cmd : args) = case cmd of
    "check" -> parseCheckArgs args
    "extract" -> parseExtractArgs args False
    -- Hack
    "extract-skip" -> parseExtractArgs args True
    c -> Left $ printf "invalid command '%s', do 'check' or 'extract'" c

-- https://hackage.haskell.org/package/cmdargs-0.10.22/docs/System-Console-CmdArgs-Implicit.html#v:modes

parseExtractArgs :: [String] -> Bool -> Result Args
parseExtractArgs args skip = case args of
    [] -> Left "empty translate args"
    [t] -> Right ArgsExtract{theorem = t, input = Std, output = Nothing, terms = [], skipCheck = skip}
    [t, f] -> Right ArgsExtract{theorem = t, input = parsePath f, output = Nothing, terms = [], skipCheck = skip}
    t : f : o : ts -> Right ArgsExtract{theorem = t, input = parsePath f, output = Just $ parsePath o, terms = ts, skipCheck = skip}

parseCheckArgs :: [String] -> Result Args
parseCheckArgs args = case args of
    [] -> Right ArgsCheck{input = Std, output = Nothing}
    [f] -> Right ArgsCheck{input = parsePath f, output = Nothing}
    [f, o] -> Right ArgsCheck{input = parsePath f, output = Just $ parsePath o}
    _ -> Left "just two args supported for command check"

parsePath :: String -> Path
parsePath "-" = Std
parsePath s = File s
module Args where

import Data.List (isPrefixOf)
import ND.ND (HypId)
import Result (Result)
import Text.Printf (printf)

data Args
    = ArgsVersion
    | ArgsCheck {input :: Path, output :: Maybe Path}
    | ArgsExtract
        { input :: Path
        , output :: Maybe Path
        , theorem :: HypId
        , terms :: [String]
        }
    deriving (Eq, Show)

validate :: Args -> Result Args
validate a@ArgsCheck{} = Right a
validate a@ArgsExtract{theorem = thm} =
    if null thm
        then Left "extract: no theorem specified"
        else Right a

data Path = Std | File FilePath
    deriving (Eq)

instance Show Path where
    show Std = "<std>"
    show (File p) = p

parseArgs :: [String] -> Result Args
parseArgs [] = Left "empty args"
parseArgs (cmd : args) = case cmd of
    "check" -> parseCheckArgs args
    "extract" -> parseExtractArgs args
    "version" -> Right ArgsVersion
    c -> Left $ printf "invalid command '%s', supported: check, extract" c

parseCheckArgs :: [String] -> Result Args
parseCheckArgs args = case args of
    [] -> Left "check: empty args"
    [f] -> Right ArgsCheck{input = parsePath f, output = Nothing}
    [f, "--out", o] ->
        Right ArgsCheck{input = parsePath f, output = Just $ parsePath o}
    [f, "-o", o] ->
        Right ArgsCheck{input = parsePath f, output = Just $ parsePath o}
    _ -> Left "check: invalid args"

parseExtractArgs :: [String] -> Result Args
parseExtractArgs (f : as)
    | isCommand f = Left $ printf "extract: expected file, not arg '%s'" f
    | otherwise =
        do
            let initialArgs =
                    ArgsExtract
                        { input = parsePath f
                        , output = Nothing
                        , theorem = ""
                        , terms = []
                        }
            args <- parseExtractArgs' as initialArgs
            validate args
parseExtractArgs _ = Left "extract: empty args"

parseExtractArgs' :: [String] -> Args -> Result Args
parseExtractArgs' [] args = Right args
parseExtractArgs' (a : as) args = case a of
    "--theorem" -> parseTheorem as args
    "-t" -> parseTheorem as args
    "--terms" -> parseTerms as args
    "-ts" -> parseTerms as args
    "--out" -> parseOut as args
    "-o" -> parseOut as args
    unex -> Left $ printf "expected arg (--theorem|-t or --terms|-ts or --out|-o) but got %s" unex

parseTheorem :: [String] -> Args -> Result Args
parseTheorem _ ArgsCheck{} = undefined
parseTheorem _ ArgsVersion{} = undefined
parseTheorem (t : as) args =
    parseExtractArgs' as (args{theorem = t})

parseTerms :: [String] -> Args -> Result Args
parseTerms _ ArgsCheck{} = undefined
parseTerms _ ArgsVersion{} = undefined
parseTerms [] args = Right args
parseTerms (t : ts) args@ArgsExtract{terms = terms} =
    if isCommand t
        then parseExtractArgs' (t : ts) args
        else parseTerms ts args{terms = terms ++ [t]}

parseOut :: [String] -> Args -> Result Args
parseOut _ ArgsVersion{} = undefined
parseOut (o : as) args =
    parseExtractArgs' as (args{output = Just $ parsePath o})

parsePath :: String -> Path
parsePath "-" = Std
parsePath s = File s

isCommand :: String -> Bool
isCommand s = "-" `isPrefixOf` s
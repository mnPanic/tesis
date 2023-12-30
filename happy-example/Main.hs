module Main (main) where

import Language
import Lexer
import Parser (calc)

main :: IO ()
main = getContents >>= print . calc . lexer

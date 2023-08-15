{- Caesar Cypher

Shiftea un número fijo todas las letras de una frase. Puede ser cualquier número
de 1 a 25

Por ej shifteando 3

    "haskell is fun" -> "kdvnhoo lv ixq"

-}
import Data.Char

-- En el libro hace esto para simplificar pero se podría para upper case también

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
--encode n xs = [ shift n x | x <- xs ]

-- forma piola yo
encode n xs = map (shift n) xs

-- el código que queda es aburrido
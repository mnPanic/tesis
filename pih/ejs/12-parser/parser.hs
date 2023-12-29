import Control.Applicative
import Data.Char

-- P es un constructor dummy para permitir que el tipo sea instancia de clases
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

item :: Parser Char
item =
    P
        ( \inp -> case inp of
            [] -> []
            (x : xs) -> [(x, xs)]
        )

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p =
        P
            ( \inp -> case parse p inp of
                [] -> []
                [(v, out)] -> [(g v, out)]
            )

{--
> parse item "abc"
[('a',"bc")]
> parse (fmap toUpper item) "abc"
[('A',"bc")]
> parse (fmap toUpper item) ""
[]
--}

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v, inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px =
        P
            ( \inp -> case parse pg inp of
                [] -> []
                [(g, out)] -> parse (fmap g px) out
            )

instance Monad Parser where
    -- >>= :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f =
        P
            ( \inp -> case parse p inp of
                [] -> []
                [(v, out)] -> parse (f v) out
            )

three :: Parser (Char, Char)
three = g <$> item <*> item <*> item
  where
    g x y z = (x, z)

three' :: Parser (Char, Char)
three' = do
    x <- item
    item
    z <- item
    return (x, z)

{--

    class Applicative f => Alternative f where
        empty :: f a
        (<|>) :: f a -> f a -> f a

--}

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (const [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) p q =
        P
            ( \inp -> case parse p inp of
                [] -> parse q inp
                res@[(v, out)] -> res
            )

sat :: (Char -> Bool) -> Parser Char
sat p = do
    c <- item
    if p c then return c else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
    char x
    string xs
    return (x : xs)

-- Many y some definidos por default en Alternative con recursión mútua, medio
-- falopa

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x : xs)

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

space :: Parser ()
space = do
    many (sat isSpace)
    return ()

int :: Parser Int
int =
    do
        char '-'
        n <- nat
        return (-n)
        <|> nat

-- Lo dejé acá
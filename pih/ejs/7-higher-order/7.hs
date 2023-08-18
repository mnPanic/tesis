-- Ejercicios
import Data.Char(digitToInt)

-- 1 [ f x | x <- xs, p x] con HOF

vals :: (a -> Bool) -> (a -> b) -> [a] -> [b]
vals p f = map f . filter p

-- ej 2

all' :: (a -> Bool) -> [a] -> Bool
all' p = (== 0) . length . (filter (not . p))

any' :: (a -> Bool) -> [a] -> Bool
any' p = (/= 0) . length . (filter p)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : (takeWhile' p xs)
                   | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile p xs
                    | otherwise = [x]

-- testeos con dropWhile' id $ (take 3 $ repeat True) ++ (take 3 $ repeat False)

-- ej 3 map y filter a partir de fold

--map' f = foldr (\x r -> f x : r) []
map' f = foldr ((:).f) []

filter' p = foldr (\x r -> (if p x then [x] else []) ++ r) []

-- ej 4 paja

-- ej 5 curry y uncurry

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x -> \y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

-- ej 6 higher order unfold, fiaca

-- ej 7 modificar transmitter para parity bits, ej 8 con faulty channel

-- ej 9 - altmap

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : (altMap g f xs)

-- ej 10 luhn con altmap

luhnDouble :: Int -> Int
luhnDouble n = if 2*n > 9 then 2*n - 9 else 2*n

luhn :: [Int] -> Bool
luhn ds = (sum (altMap id luhnDouble ds)) `mod` 10 == 0

stringToCard :: String -> [Int]
stringToCard = map digitToInt

-- luhn $ stringToCard "374245455400126" -- test amex
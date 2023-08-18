-- chap 4

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

-- condicionales (a.k.a if)

abs' :: Int -> Int
abs' n = if n > 0 then n else -n

abs'' n
    | n >= 0 = n
    | otherwise = -n

-- list patterns

test :: [Char] -> Bool
test ['a', _, _] = True
test _ = False

-- ej 1

halve :: [a] -> ([a], [a])
halve xs = (take mid xs, drop mid xs)
    where mid = length xs `div` 2

-- ej 2
third :: [a] -> a
-- a
third xs = head $ tail $ tail xs

-- b
third' xs = xs !! 2

-- c
third'' (_:_:x:_) = x

-- ej 3
safetail :: [a] -> [a]

safetail xs = if null xs then [] else tail xs
safetail' xs | null xs = []
            | otherwise = tail xs

safetail'' [] = []
safetail'' xs = tail xs

-- ejs 4, 5, 6 fiaca

-- ej 7
mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z

mult' = \x -> (\y -> (\z -> x * y * z))


-- ej 8

luhnDouble :: Int -> Int
luhnDouble n = if 2*n > 9 then 2*n - 9 else 2*n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z w = (val `mod` 10) == 0
    where val = luhnDouble x + y + luhnDouble z + w
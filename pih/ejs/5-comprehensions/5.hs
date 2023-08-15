-- ejemplos

-- primes --

factors :: Int -> [Int]
factors n = [k | k <- [1..n], n `mod` k == 0]

prime :: Int -> Bool
prime n = (length $ factors n) == 2
-- en el libro hace == [1, n] pero es más fácil long

primesLim :: Int -> [Int]
primesLim n = [x | x <- [2..n], prime x]

-- generador infinito de primos, para seleccionar con take
primes :: [Int]
primes = [x | x <- [2..], prime x]

-- positions --
positions :: Eq a => [a] -> a -> [Int]
positions xs e = [ p | (x, p) <- elemPos, x == e]
    where elemPos = zip xs [1..]
    -- nota: habia hecho zip xs [1..length xs] pero en realidad el length no
    -- hace falta, muy piola

-- ejercicios

-- ej 1

sumSquares :: Int
sumSquares = sum [ n ^2 | n <- [1..100]]

-- ej 2

grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (x, y) | x <- [0..m], y <- [0..n]]

-- ej 3
square :: Int -> [(Int, Int)]
square n = [ (x, y) | (x, y) <- grid n n, x /= y]

-- ej 4
replicate' :: Int -> a -> [a]
replicate' n x = [ x | _ <- [1..n]]

-- ej 5 
pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n],
                      x^2 + y^2 == z^2 ]

-- sin límite no anda, por qué?

-- ej 6
perfects :: Int -> [Int]
perfects n = [ k | k <- [1..n], isPerfect k]
    where isPerfect k = k == (sum $ otherFactors k)
          otherFactors k = filter (/= k) (factors k)

-- ej 7

-- [(x,y) | x <- [1,2], y <- [3,4]] con single generator y dos comprehensions
c = concat [ [(x, y) | y <- [1, 2] ] | x <- [1, 2]]

-- ejs 8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

-- 9, 10: fiaca.

positions' :: Eq a => [a] -> a -> [Int]
positions' xs e = find e elemPos
    where elemPos = zip xs [1..]

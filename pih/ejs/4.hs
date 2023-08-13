-- chap 4

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

-- condicionales (a.k.a if)

abs' :: Int -> Int
abs' n = if n > 0 then n else -n

abs' n
    | n >= 0 = n
    | otherwise = -n

-- list patterns

test :: [Char] -> Bool
test ['a', _, _] = True
test _ = False
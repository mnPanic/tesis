-- chap 2

a = b + c
    where
        b = 1
        c = 2
d = a * 2

-- ejs 1 y 2 faciles
-- ej 3
foo = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- 4 - last sin last

last' xs = xs !! ((length xs) - 1)
last'' xs = head (drop ((length xs) - 1) xs)

-- 5 - init sin init

init' xs = take (length xs - 1) xs
init'' xs = reverse (drop 1 (reverse xs))
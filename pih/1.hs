-- chapter 1

double x = x + x

-- double $ double 2 = double (double 2) -- tengo que repasar $

sum' [] = 0
sum' (n:ns) = n + sum' ns

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

-- where: definiciones locales

seqn [] = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

-- Ejercicios

-- 1
-- 2: Mostrar sum[x]= x para todo x
-- sum [x] = sum x:[] = x + sum [] = x + 0 = x

-- 3: product
product' [] = 1
product' (x:xs) = 1 * product xs

-- ver por que no anda
-- product'' = foldr (\l r -> l * r) 1
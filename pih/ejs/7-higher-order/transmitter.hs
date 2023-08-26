import Data.Char

type Bit = Int

{- Implementación mía

    bin2int :: [Bit] -> Int
    bin2int bits = sum weightedBits
        where
            weightedBits = map (\(b, p) -> b * 2^p) bitPos
            bitPos = zip bits [0..]
-}

{- Implementación del libro

Con `iterate` que produce una lista infinita aplicando una función una cantidad
incremental de veces a un valor

    iterate f x = [x, f x, f (f x), f (f (f x)), ... ]

Luego

    bit2int bits = sum [w * b | (w, b) <- zip weights bits]
        where weights = iterate (*2) 1

Pero se puede definir aún más fácil, teniendo en cuenta que si tenemos un numero
binario [a, b, c, d] aplicar bin2int produce

    (1 * a) + (2 * b) + (4 * c) + (8 * d)
    = a + 2 * (b + 2 * (c + 2 * (d + 2 * (0))))

Reemplaza cada cons por una func que suma el doble de lo que sigue
-}

-- Funciones de conversión

bin2int :: [Bit] -> Int
bin2int = foldr (\x r -> x + 2 * r) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = rem : int2bin (n `div` 2)
    where rem = n `mod` 2

-- Nos aseguramos de que los bytes tengan length 8
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- repeat :: a -> [a] da una lista infinita de copias de lo que le pasamos

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : (chop8 (drop 8 bits))

decode :: [Bit] -> String
decode = map (chr.bin2int) . chop8

-- no andaba y me tengo que ir a laburar jeje
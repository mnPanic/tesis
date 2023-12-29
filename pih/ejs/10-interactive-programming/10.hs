import Data.Char

act :: IO (Char, Char)
act = do
    x <- getChar
    getChar
    y <- getChar
    return (x, y)

getLine' :: IO String
getLine' = do
    x <- getChar
    if x == '\n'
        then return []
        else do
            xs <- getLine'
            return (x : xs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) =
    do
        putChar x
        putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do
    putStr' xs
    putChar '\n'

strlen :: IO ()
strlen = do
    putStrLn "Input a string"
    s <- getLine
    putStr "The string has "
    putStr $ show $ length s
    putStrLn " characters"

---- Ejercicios

-- 1
putStr'' :: String -> IO ()
putStr'' s = sequence_ [putChar x | x <- s]

-- 2
type Board = [Int]

putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] = do
    putRow 1 a
    putRow 2 b
    putRow 3 c
    putRow 4 d
    putRow 5 e

putRow :: Int -> Int -> IO ()
putRow row num = do
    putStr (show row)
    putStr ": "
    putStrLn (concat (replicate num "* "))

putBoard' :: Board -> IO ()
putBoard' b = putBoardFromRow' b 0

putBoardFromRow' :: Board -> Int -> IO ()
putBoardFromRow' b r =
    if r >= length b
        then return ()
        else do
            putRow (r + 1) (b !! r)
            putBoardFromRow' b (r + 1)

-- ej 3
putBoard'' :: Board -> IO ()
putBoard'' b = sequence_ [putRow n r | (n, r) <- zip [1 ..] b]

-- ej 4/5 directo
-- sequence :: [IO a] -> IO [a]
adder :: IO ()
adder = do
    putStr "Cant de numeros: "
    n <- getDigit
    ds <- sequence [getDigit | _ <- [1 .. n]]
    putStr "El total es "
    print $ show $ sum ds

getDigit :: IO Int
getDigit = do
    c <- getLine
    return (read c)

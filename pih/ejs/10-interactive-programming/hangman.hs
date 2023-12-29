import System.IO (hSetEcho, stdin)

sgetLine :: IO String
sgetLine = do
    x <- getCh
    if x == '\n'
        then do
            putChar x
            return []
        else do
            putChar '-'
            xs <- sgetLine
            return (x : xs)

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

hangman :: IO ()
hangman = do
    putStrLn "Enter secret word"
    word <- sgetLine
    play word

play :: String -> IO ()
play word = do
    putStr "? "
    guess <- getLine
    if guess == word
        then putStrLn "Win!"
        else do
            putStrLn (match word guess)
            play word

match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '_' | x <- xs]
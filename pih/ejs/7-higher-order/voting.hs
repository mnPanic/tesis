-- Algoritmos de votación

import Data.List -- sort

-- First past the post

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count e = length . filter (==e)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) =  x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort $ map (\v -> (count v vs, v)) voters
    where voters = rmdups vs

-- otra
-- result vs = sort [ (count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

--- alternative vote

ballots :: [[String]]
ballots = [
            ["Red", "Green"],
            ["Blue"],
            ["Green", "Red", "Blue"],
            ["Blue", "Green", "Red"],
            ["Green"]
          ]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (not . null)

elim :: Eq a => a -> [[a]] -> [[a]]
elim c = map (filter (==c))

rank :: Ord a => [[a]] -> [a]
rank = (map snd) . result . (map head)

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c] -> c
                (c:cs) -> winner' (elim c bs)
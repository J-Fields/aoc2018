module Day2 where

import Data.List

hasOccurrences :: Eq a => Int -> [a] -> Bool
hasOccurrences numOcc word =
    any id [length (filter (== c) word) == numOcc | c <- (nub word)]

checksum :: [String] -> Int
checksum [] = 0
checksum wordlist = 
    twos * threes
    where
        twos = length $ filter (hasOccurrences 2) wordlist
        threes = length $ filter (hasOccurrences 3) wordlist

numDifferences :: Eq a => [a] -> [a] -> Int
numDifferences xs ys =
    length . filter id $ zipWith (/=) xs ys

similarWords :: [String] -> [String]
similarWords [] = []
similarWords words =
    [x | x <- words, y <- words, x `isSimilar` y]
    where
        x `isSimilar` y =
            numDifferences x y == 1

run :: IO ()
run = do
    contents <- readFile "data/Day2.input"
    let wordlist = lines contents
    putStr "Checksum:"
    print $ checksum wordlist
    putStr "Similar words:"
    print $ similarWords wordlist

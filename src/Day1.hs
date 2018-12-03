module Day1 where

import qualified Data.Set as Set

parseInt :: String -> Int
parseInt ('+':num) = read num
parseInt num = read num

firstRepeatedFreq :: Int -> Set.Set Int -> [Int] -> Int
firstRepeatedFreq currFreq freqs (delta:rest) =
    if newFreq `Set.member` freqs
    then newFreq
    else firstRepeatedFreq newFreq (newFreq `Set.insert` freqs) rest 
    where newFreq = currFreq + delta

run :: IO ()
run = do
    contents <- readFile "data/Day1.input"
    let result = firstRepeatedFreq 0 Set.empty . cycle . map parseInt $ lines contents
    print result

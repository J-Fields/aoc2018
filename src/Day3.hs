module Day3 where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List
import Text.Scanf

type Claim = (Int, Int, Int, Int, Int)

parseClaim :: String -> Claim
parseClaim claimStr =
    (claimID, x, y, w, h)
    where
        format = fmt_ ("#" % int . " @ " % int . "," % int . ": " % int . "x" % int)
        claimID :+ x :+ y :+ w :+ h :+ () = fromJust $ scanf format claimStr

getSquares :: Claim -> [(Int, Int)]
getSquares (_, x0, y0, w, h) = [(x, y) | x <- [x0..x0+w-1], y <- [y0..y0+h-1]]

numOverlappingSquares :: [Claim] -> Int
numOverlappingSquares claims =
    length . filter (\l -> length l > 1) . group . sort . concat $ map getSquares claims
        

overlap :: Claim -> Claim -> Bool
overlap (_, x, y, w, h) (_, x2, y2, w2, h2) =
    and [x < x2+w2, x+w > x2, y < y2+h2, y+h > y2]

nonOverlappedClaims :: [Claim] -> [Claim]
nonOverlappedClaims claims =
    claims \\ overlapped
    where
        pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]
        overlapPairs = filter (uncurry overlap) $ pairs claims
        overlapped = nub . concat $ map (\(c1, c2) -> [c1, c2]) overlapPairs

run :: IO ()
run = do
    input <- readFile "data/Day3.input"
    let claims = map parseClaim $ lines input

    let numSquares = numOverlappingSquares claims
    putStr "Number of overlapping squares:"
    print numSquares

    let (claimID, _, _, _, _) = head $ nonOverlappedClaims claims
    putStr "ID of non-overlapped claim:"
    print claimID

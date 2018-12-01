{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25


runDay :: Integer -> IO ()
runDay = \case
  1 -> Day1.run
  2 -> Day2.run
  3 -> Day3.run
  4 -> Day4.run
  5 -> Day5.run
  6 -> Day6.run
  7 -> Day7.run
  8 -> Day8.run
  9 -> Day9.run
  10 -> Day10.run
  11 -> Day11.run
  12 -> Day12.run
  13 -> Day13.run
  14 -> Day14.run
  15 -> Day15.run
  16 -> Day16.run
  17 -> Day17.run
  18 -> Day18.run
  19 -> Day19.run
  20 -> Day20.run
  21 -> Day21.run
  22 -> Day22.run
  23 -> Day23.run
  24 -> Day24.run
  25 -> Day25.run

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: stack run <day>"
    (x : _) -> runDay (read x)

import System.IO
import Data.List

import qualified Data.Map as Map

import Util

solve1 :: [Int] -> Int
solve1 pos = do
  let median = pos !! ((length pos) `div` 2)
  sum $ map (\x -> abs $ x - median) pos

gaussSum :: Int -> Int
gaussSum n = n * (n+1) `div` 2

trySolve2 :: [Int] -> Int -> Int
trySolve2 pos num = sum $ map (\x -> gaussSum $ abs $ x - num) pos

solve2 :: [Int] -> Int
solve2 pos = do
  minimum $ map (\x -> trySolve2 pos x) [head pos .. last pos]


main :: IO ()
main = do
  content <- readFile "day07/input.txt"

  let positions = sort $ map readInt $ splitOn "," content
  putStrLn $ show $ solve1 positions
  putStrLn $ show $ solve2 positions





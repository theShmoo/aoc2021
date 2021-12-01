import System.IO
import Data.Char
import Data.List

readInt :: String -> Int
readInt str = read str :: Int

solve1 :: [String] -> Int
solve1 allLines = do
  let ints = map readInt allLines
  let zipped = zip ints (drop 1 ints)
  sum [1 | (val, next) <- zipped, val < next]


solve2 :: [String] -> Int
solve2 allLines = do
  let ints = map readInt allLines
  let tripples = transpose [ints, (drop 1 ints), (drop 2 ints)]
  let zipped = zip tripples (drop 1 tripples)
  sum [1 | (val, next) <- zipped, (sum val) < (sum next)]

main = do
  content <- readFile "input.txt"
  let allLines = lines content
  print . solve1 $ allLines
  print . solve2 $ allLines


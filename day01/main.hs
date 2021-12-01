import System.IO
import Data.Char
import Data.List

readInt :: String -> Int
readInt str = read str :: Int

solve1 :: [Int] -> Int
solve1 allLines = do
  let zipped = zip ints (drop 1 ints)
  sum [1 | (val, next) <- zipped, val < next]

solve2 :: [Int] -> Int
solve2 allLines = do
  let tripples = transpose [ints, (drop 1 ints), (drop 2 ints)]
  let zipped = zip tripples (drop 1 tripples)
  sum [1 | (val, next) <- zipped, (sum val) < (sum next)]

main = do
  content <- readFile "input.txt"
  let ints = map readInt allLines
  let allLines = lines content
  print . solve1 $ allLines
  print . solve2 $ allLines


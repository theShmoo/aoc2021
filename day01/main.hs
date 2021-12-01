import System.IO
import Data.Char
import Data.List

readInt :: String -> Int
readInt str = read str :: Int

solve1 :: [Int] -> Int
solve1 xs = do
  let zipped = zip xs (drop 1 xs)
  sum [1 | (val, next) <- zipped, val < next]

solve2 :: [Int] -> Int
solve2 xs = do
  let tripples = transpose [xs, (drop 1 xs), (drop 2 xs)]
  let zipped = zip tripples (drop 1 tripples)
  sum [1 | (val, next) <- zipped, (sum val) < (sum next)]

main = do
  content <- readFile "input.txt"
  let ints = map readInt $ lines content
  print . solve1 $ ints
  print . solve2 $ ints


import System.IO
import Data.Char

solve :: Int -> String -> Int
solve n str = do
  let ints = map digitToInt str
  let zipped = zip ints $ rotate n ints
  sum [val | (val, next) <- zipped, val == next]


rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

main = do
  content <- readFile "input.txt"
  let firstLine = lines content !! 0
  print . solve 1 $ firstLine
  print . solve (length firstLine `div` 2) $ firstLine


import System.IO
import Data.Char
import Data.List

readInt :: String -> Int
readInt str = read str :: Int

parseInput :: String -> [(String, Int)]
parseInput str = do
  let strLines = lines str
  let strWords = map words strLines
  [(ws !! 0, readInt (ws !! 1)) | ws <- map words strLines]

solve1 :: [(String, Int)] -> Int
solve1 xs = do
  let down = sum [val | (cmd, val) <- xs, cmd == "down"]
  let up = sum [val | (cmd, val) <- xs, cmd == "up"]
  let forward = sum [val | (cmd, val) <- xs, cmd == "forward"]
  (down - up) * forward

solve2 :: [(String, Int)] -> Int
solve2 xs = do
  let aim = [if (cmd == "down") then val else if (cmd == "up") then -val else 0 | (cmd, val) <- xs]
  let summed_aims = scanl1 (+) aim
  let forward = [if (cmd == "forward") then val else 0 | (cmd, val) <- xs]
  let horz = sum forward
  let depth = sum (zipWith (*) summed_aims forward)
  horz * depth

main = do
  content <- readFile "input.txt"
  let input = parseInput content
  print $ solve1 input
  print $ solve2 input


import System.IO
import Data.Char
import Data.List

readInt :: String -> Int
readInt str = read str :: Int

parseInput :: String -> [[Int]]
parseInput str = do
  transpose $ map (map digitToInt) $ lines str

binToDec :: [Bool] -> Int
binToDec = foldr (\x y -> fromEnum x + 2*y) 0

decToBin :: [Int] -> [Bool]
decToBin = map (>0)

countElem e = length . filter (e ==)

minMax x = ((countElem 0 x), (countElem 1 x))

moreOnes x = (countElem 0 x) <= (countElem 1 x)

reverseList xs = foldl (\x y -> y:x) [] xs

boolList t = [z > o | (z, o) <- t]

solve1 xs = do
  let bin = reverseList $ boolList $ map minMax xs
  let epsilonRate = binToDec bin
  let gammaRate = binToDec $ map not bin
  epsilonRate * gammaRate

oxRating i xs
  | length xs == 1 = xs
  | otherwise = oxRating (i+1) $ filter (\s -> s!!i == most) $ xs
  where most = fromEnum $ moreOnes $ map (!!i) xs

co2Rating i xs
  | length xs == 1 = xs
  | otherwise = co2Rating (i+1) $ filter (\s -> s!!i /= most) $ xs
  where most = fromEnum $ moreOnes $ map (!!i) xs

solve2 xs = do
  -- binToDec $ decToBin
  let input = transpose xs
  let co2 = binToDec $ decToBin $ reverseList $ head $ co2Rating 0 input
  let ox = binToDec $ decToBin $ reverseList $ head $ oxRating 0 input
  ox * co2

main = do
  content <- readFile "input.txt"
  let input = parseInput content
  print $ solve1 input
  print $ solve2 input


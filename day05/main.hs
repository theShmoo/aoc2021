import System.IO
import Data.Char
import Data.List
import Data.Tuple
import Data.Maybe
import Util

x1 x = fst $ fst x
y1 x = snd $ fst x

x2 x = fst $ snd x
y2 x = snd $ snd x

splitLeftRight :: String -> (String,String)
splitLeftRight input = tuplify2 $ splitOn " -> " input

splitComma :: String -> (Int,Int)
splitComma input = (\x -> (readInt $ fst x, readInt $ snd x)) $ tuplify2 $ splitOn "," input

splitFromTo :: String -> ((Int,Int),(Int,Int))
splitFromTo input = (\x -> (splitComma $ fst x, splitComma $ snd x)) $ splitLeftRight input

buildRow :: (Int, Int) -> Int -> [Int]
buildRow pos max = do
  let vals = [fst pos, snd pos]
  let low = minimum vals
  let high = maximum vals
  replicate low 0 ++ replicate (high-low+1) 1 ++ replicate (max-high-1) 0

buildDiagRow :: Int -> Int -> ((Int,Int),(Int,Int)) -> [Int]
buildDiagRow y maxX pos = do

  let sortPos = if y1 pos < y2 pos then pos else (snd pos, fst pos)

  let ((_x1, _y1), (_x2, _y2)) = sortPos

  let min = minimum [_x1, _x2]
  let pos = if _x1 < _x2 then _x1 + (y - _y1) else _x1 - (y - _y1)

  if y < _y1 || y > _y2
    then replicate maxX 0
  else
    buildRow (pos, pos) maxX

insertRowIntoGrid :: (Int, Int) -> Int -> (Int, Int) -> [[Int]]
insertRowIntoGrid range row pos = do
  let (maxX, maxY) = range
  grid maxX row 0 ++ [buildRow pos maxY] ++ grid maxX (maxY-row-1) 0

insertColIntoGrid :: (Int, Int) -> Int -> (Int, Int) -> [[Int]]
insertColIntoGrid range col pos = transpose $ insertRowIntoGrid (swap range) col pos

insertDiagIntoGrid :: (Int, Int) -> ((Int,Int),(Int,Int)) -> [[Int]]
insertDiagIntoGrid range pos = do
  let (maxX, maxY) = range
  map (\y -> buildDiagRow y maxX pos) [0..maxY-1]

getMinMax :: [((Int,Int),(Int,Int))] -> (Int,Int)
getMinMax instr = (
  (maximum $ map x1 instr ++ map x2 instr) + 1,
  (maximum $ map y1 instr ++ map y2 instr) + 1
  )

toGrid ::  (Int, Int) -> ((Int,Int),(Int,Int)) -> [[Int]]
toGrid range input = do
  let ((_x1, _y1), (_x2, _y2)) = input
  if _x1 == _x2
    then insertColIntoGrid range _x1 (_y1,_y2)
    else insertRowIntoGrid range _y1 (_x1,_x2)

toGridWithDiag ::  (Int, Int) -> ((Int,Int),(Int,Int)) -> [[Int]]
toGridWithDiag range input = do
  let ((_x1, _y1), (_x2, _y2)) = input
  if _x1 == _x2
    then insertColIntoGrid range _x1 (_y1,_y2)
  else if _y1 == _y2
    then insertRowIntoGrid range _y1 (_x1,_x2)
  else
    insertDiagIntoGrid range input

addGrids :: [[Int]] -> [[Int]] -> [[Int]]
addGrids lhs rhs = zipWith (\l r -> zipWith (+) l r) lhs rhs

solve1 :: [((Int,Int),(Int,Int))] -> Int
solve1 inst = do
  let filtered = filter (\x -> x1 x == x2 x || y1 x == y2 x) inst

  let range = getMinMax filtered
  let coords = map (\x -> toGrid range x) filtered

  let comb = foldl addGrids (head coords) (tail coords)
  length $ filter (> 1) $ concat comb

solve2 :: [((Int,Int),(Int,Int))] -> Int
solve2 inst = do
  let range = getMinMax inst
  let coords = map (\x -> toGridWithDiag range x) inst

  let comb = foldl addGrids (head coords) (tail coords)
  length $ filter (> 1) $ concat comb


main :: IO ()
main = do
  content <- readFile "day05/input.txt"
  let inst = map splitFromTo $ lines content
  putStrLn $ show $ solve1 inst
  putStrLn $ show $ solve2 inst





import System.IO
import Data.List

import qualified Data.Map as Map

import Util

day :: Map.Map Int Int -> Map.Map Int Int
day fish = Map.fromList
    [ case i of
        6 -> (6, (fishAt 7) + (fishAt 0))
        8 -> (8, fishAt 0)
        _ -> (i, fishAt (i + 1))
    | i <- [0 .. 8]
    ]
  where
    fishAt pos = Map.findWithDefault 0 pos fish


solve :: Int -> Map.Map Int Int -> Int
solve n fish = Map.foldl (+) 0 (iterate day fish !! n)

main :: IO ()
main = do
  content <- readFile "day06/input.txt"

  let fish = occur $ sort $ map readInt $ splitOn "," content
  putStrLn $ show $ solve 80 fish
  putStrLn $ show $ solve 256 fish





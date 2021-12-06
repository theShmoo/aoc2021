module Util where

import Data.List
import qualified Data.Map as Map

splitS :: (String -> Bool) -> String -> [String]
splitS pred s = case dropWhile pred (init $ tails s) of
  [] -> []
  s' -> (map head w) : s'''
    where
      (w, s'') = break pred s'
      s''' = case s'' of
               (s''':_) -> splitS pred s'''
               [] -> []

split :: (Char -> Bool) -> String -> [String]
split p = splitS (p . head)

splitOn :: String -> String -> [String]
splitOn ss s = let (h : tail) = splitS (isPrefixOf ss) s
               in h : (map (drop (length ss - 1)) tail)

readInt :: String -> Int
readInt str = read str :: Int

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

grid :: Int -> Int -> a -> [[a]]
grid x y = replicate y . replicate x

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

occur :: Ord a => [a] -> Map.Map a Int
occur xs = Map.fromListWith (+) ([(x, 1) | x <- xs])

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

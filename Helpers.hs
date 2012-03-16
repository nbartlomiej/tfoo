module Helpers where

import Data.List

replace :: Int -> a -> [a] -> [a]
replace index element list = (take index list) ++ [element] ++ (drop (index+1) list)

findList :: (Eq a) => [a] -> [a] -> Bool
findList sequence list = sequence `elem` concat (map inits (tails list))

type Matrix a = [[a]]

replace' :: Int -> Int -> a -> Matrix a -> Matrix a
replace' x y element matrix = replace x (replace y element (matrix !! x)) matrix

diagonal :: Matrix a -> [a]
diagonal m = zipWith (!!) m [0..]

diagonals :: Matrix a -> [[a]]
diagonals matrix =
  let tails' = tail . tails
      diagonalsNW m = map diagonal ([m] ++ tails' m ++ tails' (transpose m))
  in diagonalsNW matrix ++ diagonalsNW (map reverse matrix)


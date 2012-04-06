module Tfoo.Matrix where

import Data.List

replace :: Int -> a -> [a] -> [a]
replace index element list = (take index list) ++ [element] ++ (drop (index+1) list)

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

differences :: (Eq a) => Matrix a -> Matrix a -> [(Int, Int)]
differences a b =
  let enumeratedRows = zip3 a b [0..]
      enumerateEntry (a,b,x) = zip4 a b (repeat x) [0..]
      enumeratedEntries = concat $ map enumerateEntry enumeratedRows
      compareEntries a b = fst a == fst b && snd a == snd b
      different (a,b,_,_) = a /= b
      differentEntries = filter different enumeratedEntries
      entryCoordinates (_,_,x,y) = (x,y)
  in  map entryCoordinates differentEntries

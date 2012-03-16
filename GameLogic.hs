module GameLogic where

import Data.List
import Helpers

data Mark = O | X deriving (Eq, Show)
type Cell = Maybe Mark

type Pattern   = [Cell]
type Board     = Matrix Cell

getCell :: Board -> Int -> Int -> Cell
getCell board x y = (((board) !! x) !! y)

generateBoard :: Int -> Board
generateBoard size = [ [Nothing | x <- [1..size]] | y <- [1..size]]

patterns :: Board -> [Pattern]
patterns board = board ++ (transpose board) ++ (diagonals board)

winner :: Board -> Maybe Mark
winner board
  | any (findList [Just O, Just O, Just O, Just O, Just O]) (patterns board) = Just O
  | any (findList [Just X, Just X, Just X, Just X, Just X]) (patterns board) = Just X
  | otherwise = Nothing

nextMark :: Board -> Mark
nextMark board = if (count X) <= (count O) then X else O where
  count mark = length $ filter (Just mark == ) $ concat board

module Tfoo.Board where

import Tfoo.Matrix

import Data.List

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

aiResponse :: Board -> (Int, Int)
aiResponse board =
  let b = maximumBy (\a b -> compare (evaluate a) (evaluate b)) $ aiMoves board
  in head $ difference board b

aiMoves :: Board -> [Board]
aiMoves board =
  let lengths  = [0 .. (length board - 1)]
      available x y = board !! x !! y == Nothing
  in  [ replace' x y (Just X) board | x <- lengths, y <- lengths, available x y ]

evaluate :: Board -> Int
evaluate board =
  let knowledge = [
        -- Not the brightest AI, has only few rules; add more if you wish!
        ( [Just O , Just O   , Just O   , Just O   , Nothing] , -1000 ),
        ( [Just O , Just O   , Just O   , Nothing  , Just O]  , -1000 ),
        ( [Just O , Just O   , Nothing  , Just O   , Just O]  , -1000 ),
        ( [Just O , Just O   , Just O   , Nothing]            , -100 ),
        ( [Just X , Just X   , Just X   , Just X   , Just X]  , 1000000 ),
        ( [Just X , Just X  , Just X  , Just X  , Nothing]    , 100 ),
        ( [Just X , Just X  , Just X  , Nothing , Nothing]    , 4 ),
        ( [Just X , Just X  , Nothing , Nothing , Nothing]    , 2 ),
        ( [Just X , Nothing , Nothing , Nothing , Nothing]    , 1 )
        ]
      ratePattern' p = map (\(k,s) -> if (findList k p) then s else 0) knowledge
      ratePattern p = (ratePattern' p) ++ (ratePattern' $ reverse p)
  in sum $ concat $ map ratePattern (patterns board)

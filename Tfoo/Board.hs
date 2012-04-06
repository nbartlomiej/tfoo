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
  | any (isInfixOf [Just O, Just O, Just O, Just O, Just O]) (patterns board) = Just O
  | any (isInfixOf [Just X, Just X, Just X, Just X, Just X]) (patterns board) = Just X
  | otherwise = Nothing

nextMark :: Board -> Mark
nextMark board = if (count X) <= (count O) then X else O where
  count mark = length $ filter (Just mark == ) $ concat board

aiResponse :: Board -> (Int, Int)
aiResponse board =
  let addScore board = (board, evaluate board)
      compareScores a b = compare (snd a) (snd b)
      removeScore = fst
      best = removeScore $ maximumBy compareScores $ map addScore $ aiMoves board
  in  head $ differences board best

aiMoves :: Board -> [Board]
aiMoves board =
  let lengths = [0 .. (length board - 1)]
      available x y = board !! x !! y == Nothing
  in  [ replace' x y (Just O) board | x <- lengths, y <- lengths, available x y ]

evaluate :: Board -> Int
evaluate board =
  let knowledge' = [
        -- Not the brightest AI, has only few rules; add more if you wish!
        ( [Just X , Just X  , Just X  , Just X  , Nothing] , -1000 ),
        ( [Just X , Just X  , Just X  , Nothing , Just X]  , -1000 ),
        ( [Just X , Just X  , Nothing , Just X  , Just X]  , -1000 ),
        ( [Nothing, Just X , Just X  , Just X  , Nothing]  , -100 ),
        ( [Nothing, Just X , Just X  , Nothing , Just X, Nothing] , -100 ),
        ( [Nothing, Just X , Nothing  , Nothing , Nothing] , -2),
        ( [Just O , Just O  , Just O  , Just O  , Just O]  , 1000000 ),
        ( [Just O , Just O  , Just O  , Just O  , Nothing] , 100 ),
        ( [Nothing, Just O , Just O  , Just O   , Nothing] , 5 ),
        ( [Nothing, Just O , Just O  , Nothing  , Nothing] , 3 ),
        ( [Nothing, Just O , Nothing , Nothing , Nothing] , 1 )
        ]
      reversePatterns (pattern, score) = (reverse pattern,score)
      knowledge = knowledge' ++ (map reversePatterns knowledge')
      ratePattern p = map (\(k,s) -> if (isInfixOf k p) then s else 0) knowledge
      knownPatterns = (map fst knowledge)
      hasKnownPattern pattern = any (\y -> isInfixOf y pattern ) knownPatterns
  in sum $ concat $ map ratePattern $ filter hasKnownPattern (patterns board)

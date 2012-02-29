module Takefive where

import List
import Control.Monad

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

data Mark = O | X deriving (Eq, Show)
type Cell = Maybe Mark

type Pattern   = [Cell]
type Board     = Matrix Cell

generateBoard :: Int -> Board
generateBoard size = [ [Nothing | x <- [1..size]] | y <- [1..size]]

patterns :: Board -> [Pattern]
patterns board = board ++ (transpose board) ++ (diagonals board)

winner :: Board -> Maybe Mark
winner board
  | any (findList [Just O, Just O, Just O, Just O, Just O]) (patterns board) = Just O
  | any (findList [Just X, Just X, Just X, Just X, Just X]) (patterns board) = Just X
  | otherwise = Nothing

aiMove :: Board -> Board
aiMove board = maximumBy (\a b -> compare (evaluate a) (evaluate b)) $ aiMoves board

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

-- main = play (generateBoard 11)

display :: Board -> IO ()
display board = do
  let display' c = if c==Nothing then " _ " else if c==(Just O) then " o " else " x "
      spacedIndex = [(show x) ++ " " | x <- [0..9]] ++ (map show [10..(length board)])
  mapM_ putStr (["   "] ++ (intersperse " " (tail spacedIndex)) ++  ["\n"])
  mapM_ ( \ (index, row) -> do
    putStr $ (spacedIndex !! index) ++ (concat (map display' row))  ++ "\n"
    ) (zipWith (\ a b -> (a,b)) [1..] board)

play :: Board -> IO ()
play board
  | winner board == (Just O) = display board >> putStrLn "You have won."
  | winner board == (Just X)  = display board >> putStrLn "The computer has won."
  | otherwise = do
      display board >> putStrLn "Input coordinates in format: x,y."
      input <- getLine
      let (x,y) = break (==',') input
          ix = (read x) -1
          iy = (read $ tail y) -1
          board' = replace' ix iy (Just O) board
      play (if winner board' == Nothing then aiMove board' else board')

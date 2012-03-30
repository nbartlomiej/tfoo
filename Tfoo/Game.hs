module Tfoo.Game where

import Tfoo.Board

import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)
import Control.Concurrent.Chan

type Player = String

data Game = Game {
  playerX :: Maybe Player,
  playerO :: Maybe Player,
  channel :: Chan ServerEvent,
  board   :: Board
}

setPlayer :: Game -> Mark -> Player -> Game
setPlayer game O playerId = game { playerO = Just playerId }
setPlayer game X playerId = game { playerX = Just playerId }

whoseTurn :: Game -> Maybe Player
whoseTurn g = if nextMark (board g) == O then playerO g else playerX g

gameState :: Game -> String
gameState game =
  let board' = board game
  in  maybe (nextMove board') announceWinner (winner board')
  where nextMove board = (show $ nextMark board) ++ "'s turn"
        announceWinner mark = "Game won: " ++ (show mark)

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

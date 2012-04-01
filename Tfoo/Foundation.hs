module Tfoo.Foundation where

import Tfoo.Game
import Yesod.Static
import Control.Concurrent.MVar

data Tfoo = Tfoo {
    seed       :: Int,
    games      :: MVar [IO Game],
    nextGameId :: MVar Int,
    tfooStatic :: Static
  }

-- Increment Tfoo's Game counter and return id of the next new Game.
newGame :: Tfoo -> IO Int
newGame tfoo =
    modifyMVar (nextGameId tfoo) incrementMVar
  where
    incrementMVar :: Int -> IO (Int, Int)
    incrementMVar value = return (value+1, value)

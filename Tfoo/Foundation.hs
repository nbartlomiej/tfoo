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


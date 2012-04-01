{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

module Tfoo.Foundation where

import Tfoo.Game

import Yesod
import Yesod.Static

import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Text.Julius (juliusFile)

import Control.Concurrent.MVar
import Data.Text as T

data Tfoo = Tfoo {
    seed       :: Int,
    games      :: MVar [IO Game],
    nextGameId :: MVar Int,
    tfooStatic :: Static
  }

mkYesodData "Tfoo" [parseRoutes|
/                           HomeR GET
/games                      GamesR POST
/games/#Int                 GameR GET
/games/#Int/join/o          PlayerOR POST
/games/#Int/join/x          PlayerXR POST
/games/#Int/mark/#Int/#Int  MarkR POST
/games/#Int/listen          ChannelR GET
/static                     StaticR Static tfooStatic
|]

instance Yesod Tfoo where
  defaultLayout widget = do
    pageContent <- widgetToPageContent $ do
      widget
      addStylesheet $ StaticR $ StaticRoute ["styles", "tfoo.css"] []
      addScript $ StaticR $ StaticRoute ["scripts","jquery-1.7.1.min.js"] []
      addScript $ StaticR $ StaticRoute ["scripts","jquery.wiggle.js"] []
    hamletToRepHtml $(hamletFile "Tfoo/Assets/Templates/layout.hamlet")


-- Increment Tfoo's Game counter and return id of the next new Game.
newGame :: Tfoo -> IO Int
newGame tfoo =
    modifyMVar (nextGameId tfoo) incrementMVar
  where
    incrementMVar :: Int -> IO (Int, Int)
    incrementMVar value = return (value+1, value)

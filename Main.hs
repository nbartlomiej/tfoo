{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
module Main where

import Tfoo.Foundation
import Tfoo.Board
import Tfoo.Game

import Application
import Tfoo.Handlers.Root

import Data.List

import Yesod
import Yesod.Static

import Control.Concurrent.MVar
import Control.Concurrent.Chan

import System.Random as Random
import System.Environment (getArgs)

mkYesodDispatch "Tfoo" resourcesTfoo


createGame :: IO Game
createGame = do
  channel <- newChan
  return Game {
    playerO = Nothing,
    playerX = Nothing,
    channel = channel,
    board   = generateBoard 20
  }

gameStream :: [IO Game]
gameStream = repeat createGame

main :: IO ()
main = do
  nextGameId <- newMVar 1
  games   <- newMVar gameStream
  seedP   <- liftIO $ Random.getStdGen >>= (\x -> return $ next x)
  static' <- static "static"
  args    <- getArgs
  warpDebug (getPort args) (Tfoo (fst seedP) games nextGameId static')

getPort :: [String] -> Int
getPort args = extractPort $ "-p" `elemIndex` args
  where extractPort (Just index) = read $ args !! (index+1)
        extractPort (Nothing) = 3100

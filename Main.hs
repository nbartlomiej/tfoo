{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
module Main where

import Tfoo.Foundation
import Tfoo.Matrix
import Tfoo.Board
import Tfoo.Game

import Tfoo.Handlers.Root

import Data.Text as T
import Data.List as L
import Data.Maybe as M
import Data.Monoid
import System.Random as Random
import Control.Monad

import Control.Concurrent.MVar

import Yesod
import Yesod.Static
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Text.Julius (juliusFile)

import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)
import Control.Concurrent.Chan
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

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

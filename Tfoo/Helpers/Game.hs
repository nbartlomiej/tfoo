module Tfoo.Helpers.Game where

import Application
import Tfoo.Helpers.Application
import Tfoo.Game
import Tfoo.Board
import Tfoo.Matrix
import Tfoo.Foundation

import Data.Text hiding (map, intercalate, words)
import Data.List
import Data.Maybe

import Yesod

import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

getGame :: Int -> Handler Game
getGame id = do
  tfoo <- getYesod
  maxId <- liftIO $ readMVar $ nextGameId tfoo
  list  <- liftIO $ readMVar $ games tfoo
  if id < maxId
    then (liftIO $ (list) !! id) >>= (\game -> return game)
    else notFound

updateGame :: Int -> Game -> Handler ()
updateGame id game = do
  tfoo <- getYesod
  liftIO $ modifyMVar (games tfoo) (\games ->
      return (Tfoo.Matrix.replace id (return game) games, games)
    )
  return ()

joinGame :: Int -> Mark -> Handler ()
joinGame id mark =
  do
    game <- getGame id
    tfoo <- getYesod
    appendSession' (pack "players") $ pack (playerId tfoo)
    updateGame id $ setPlayer game mark (playerId tfoo)
    return ()
  where
    playerId tfoo = (show $ seed tfoo) ++ (show id) ++ (show mark)

newSinglePlayerGame :: Int -> Handler ()
newSinglePlayerGame id = do
  joinGame id X
  addAi id O
  return ()

addAi :: Int -> Mark -> Handler ()
addAi id mark = do
  game <- getGame id
  updateGame id $ setPlayer game mark "AI"
  return ()

broadcast :: Int -> String -> [(String, String)] -> Handler ()
broadcast gameId messageId pairs = do
  game <- getGame gameId
  liftIO $ writeChan (channel game) $ serverEvent $ return $ fromString message
  where message = "{"++(stringifiedPairs $ ("id",messageId):pairs)++"}"
        stringifiedPairs pairs = intercalate ", " $ map stringifyPair pairs
        stringifyPair p = "\""++(fst p) ++ "\": \"" ++ (snd p) ++ "\""
        serverEvent = ServerEvent Nothing Nothing

validMove :: Int -> Int -> Game -> [Player] -> Bool
validMove x y game authorizations =
  let whoseTurn' = whoseTurn game
      board' = board game
      gameInProgress = (winner board') == Nothing
      targetCellEmpty = (getCell board' x y) == Nothing
      playerAuthorized =
        fromMaybe False $ liftM (`elem` authorizations) whoseTurn'
  in gameInProgress && targetCellEmpty && playerAuthorized

placeMark :: Int -> Int -> Int -> Handler ()
placeMark id x y = do
    game   <- getGame id
    board' <- return $ board game
    mark   <- return $ nextMark board'
    updateGame id $ game {board = replace' x y (Just mark) board'}

    game' <- getGame id
    broadcast id "mark-new" [("x", show x), ("y", show y), ("mark", show mark)]
    broadcast id "alert" [("content", gameState game')]

playerAuthorizations :: Handler [Player]
playerAuthorizations = do
  authorizations <- lookupSession $ pack "players"
  return $ fromMaybe [] $ fmap (words . unpack) authorizations

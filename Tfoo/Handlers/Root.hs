{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

module Tfoo.Handlers.Root where

import Tfoo.Foundation
import Tfoo.Matrix
import Tfoo.Board
import Tfoo.Game

import Application

import Data.Text as T
import Data.List as L
import Data.Maybe as M
import Data.Monoid
import System.Random as Random
import Control.Monad

import Control.Concurrent.MVar

import Yesod
import Yesod.Default.Util
import Yesod.Static
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)
import Text.Julius (juliusFile)

import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)
import Control.Concurrent.Chan
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

getHomeR :: Handler RepHtml
getHomeR = do
  tfoo <- getYesod
  defaultLayout $(widgetFileNoReload "index")

postGamesR :: Handler RepHtml
postGamesR = do
    tfoo <- getYesod
    id   <- liftIO $ newGame tfoo
    redirect $ GameR id

getGameR :: Int -> Handler RepHtml
getGameR id = let
    columns = [0..19]
    rows    = [0..19]
  in do
    game <- getGame id
    maybePlayers <- lookupSession $ T.pack "players"
    tfoo <- getYesod
    defaultLayout $(widgetFileNoReload "game")

postMarkR :: Int -> Int -> Int -> Handler ()
postMarkR id x y = do
    game               <- getGame id
    whoseTurn'         <- return $ whoseTurn game
    board'             <- return $ board game
    userAuthorizations <- do
      authorizations <- lookupSession $ T.pack "players"
      return $ fmap (L.words . T.unpack) authorizations

    -- The target cell has to be empty.
    require $ (getCell (board game) x y) == Nothing
    -- User has to be authorized to make this move
    require $ fromMaybe False (liftM2 elem whoseTurn' userAuthorizations)
    -- The game has to be still in progress
    require $ (winner board') == Nothing

    updateGame id $ game {board = replace' x y (Just $ nextMark board') board'}
    game' <- getGame id

    broadcast id "mark-new" [
        ("x", show x), ("y", show y), ("mark", show (nextMark board'))
      ]

    broadcast id "alert" [("content", gameState game')]

  where require result = if result == False
          then permissionDenied $ T.pack "Permission Denied"
          else return ()
        elem' x y = (elem . L.words . T.unpack)
        userAuthorizations' = L.words . T.unpack

postPlayerOR :: Int -> Handler RepHtml
postPlayerOR id = do
  game <- getGame id
  if (playerO game) == Nothing
    then do
      joinGame id O
      broadcast id "player-new" [("side", "O")]
      broadcast id "alert" [("content", "Player joined: Circle")]
      return ()
    else return ()
  redirect $ GameR id

postPlayerXR :: Int -> Handler RepHtml
postPlayerXR id = do
  game <- getGame id
  if (playerX game) == Nothing
    then do
      joinGame id X
      broadcast id "player-new" [("side", "X")]
      broadcast id "alert" [("content", "Player joined: Cross")]
      return ()
    else return ()
  redirect $ GameR id

getChannelR :: Int -> Handler ()
getChannelR id = do
  game <- getGame id
  channel <- liftIO $ dupChan $ channel game
  request  <- waiRequest
  response  <- lift $ eventSourceApp channel request
  updateGame id game
  sendWaiResponse response

broadcast :: Int -> String -> [(String, String)] -> Handler ()
broadcast gameId messageId pairs = do
  game <- getGame gameId
  liftIO $ writeChan (channel game) $ serverEvent $ return $ fromString message
  where message = "{"++(stringifiedPairs $ ("id",messageId):pairs)++"}"
        stringifiedPairs pairs = L.intercalate ", " $ L.map stringifyPair pairs
        stringifyPair p = "\""++(fst p) ++ "\": \"" ++ (snd p) ++ "\""
        serverEvent = ServerEvent Nothing Nothing

joinGame :: Int -> Mark -> Handler ()
joinGame id mark =
  do
    game <- getGame id
    tfoo <- getYesod
    appendSession' (T.pack "players") $ T.pack (playerId tfoo)
    updateGame id $ setPlayer game mark (playerId tfoo)
    return ()
  where
    playerId tfoo = (show $ seed tfoo) ++ (show id) ++ (show mark)

-- Appends the given value to the session key.
appendSession :: Text -> Text -> Handler ()
appendSession name value = do
  initial <- lookupSession name
  setSession name $ fromJust $ initial `mappend` Just value

-- Appends the given value to the session key, inserts space before the value.
appendSession' :: Text -> Text -> Handler ()
appendSession' name value = appendSession name (T.pack " " `mappend` value)

getGame :: Int -> Handler Game
getGame id = do
  tfoo <- getYesod
  maxId <- liftIO $ readMVar $ nextGameId tfoo
  list  <- liftIO $ readMVar $ games tfoo
  if id < maxId
    then (liftIO $ (list) !! id) >>= (\game -> return game)
    else notFound

-- todo: refactor
updateGame :: Int -> Game -> Handler ()
updateGame id game = do
  tfoo <- getYesod
  liftIO $ modifyMVar (games tfoo) (\games ->
      return (Tfoo.Matrix.replace id (return game) games, games)
    )
  return ()



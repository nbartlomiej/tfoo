{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes           #-}

module Tfoo.Handlers.Root where

import Tfoo.Foundation
import Tfoo.Matrix
import Tfoo.Board
import Tfoo.Game

import Application
import Tfoo.Helpers.Application
import Tfoo.Helpers.Game

import Data.Text as T
import Data.List as L
import Data.Maybe as M

import Control.Monad
import Control.Applicative
import Control.Concurrent.MVar

import Yesod
import Yesod.Default.Util
import Text.Julius

import Control.Concurrent.Chan
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppChan)
import Data.Default
import Network.Wai.Internal (ResponseReceived(..))


getHomeR :: Handler Html
getHomeR = do
  tfoo <- getYesod
  defaultLayout $(widgetFileNoReload def "index")

postGamesR :: Handler Html
postGamesR = do
    tfoo <- getYesod
    id   <- liftIO $ newGame tfoo
    single <- runInputPost $ iopt hiddenField (T.pack "single")
    if single == Just ("true" :: String)
      then newSinglePlayerGame id
      else return ()
    redirect $ GameR id

getGameR :: Int -> Handler Html
getGameR id = let
    columns = [0..19]
    rows    = [0..19]
  in do
    game <- getGame id
    maybePlayers <- lookupSession $ T.pack "players"
    tfoo <- getYesod
    defaultLayout $(widgetFileNoReload def "game")

postMarkR :: Int -> Int -> Int -> Handler ()
postMarkR id x y = do
    game   <- getGame id
    board' <- return $ board game
    authorizations <- playerAuthorizations

    require $ validMove x y game authorizations
    placeMark id x y

    game' <- getGame id

    if (nextMark $ board game') == O && (playerO game') == (Just "AI")
      then let (x', y') = aiResponse (board game')
           in do
             require $ validMove x' y' game' ["AI"]
             placeMark id x' y'
      else return ()

  where require result = if result == False
          then permissionDenied $ T.pack "Permission Denied"
          else return ()

postPlayerOR :: Int -> Handler Html
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

postPlayerXR :: Int -> Handler Html
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
  sendWaiApplication (eventSourceAppChan channel)
  updateGame id game

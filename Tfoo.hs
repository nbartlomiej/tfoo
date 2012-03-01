{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod
import Takefive
import Control.Concurrent.Chan
import Data.Text as T
import Data.List as L
import Data.Maybe as M
import Control.Concurrent.MVar as V
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)

data Game = Game {
  playerX :: Maybe String,
  playerO :: Maybe String,
  channel :: Chan ServerEvent,
  board   :: Board
}

setPlayer :: Game -> Mark -> String -> Game
setPlayer game O playerId = game { playerO = Just playerId }
setPlayer game X playerId = game { playerX = Just playerId }

data Tfoo = Tfoo {
    games      :: MVar [IO Game],
    nextGameId :: MVar Int
  }

mkYesod "Tfoo" [parseRoutes|
/                       HomeR GET
/games                  GamesR POST
/games/#Int             GameR GET
/games/#Int/join/o      PlayerOR POST
/games/#Int/join/x      PlayerXR POST
/games/#Int/listen      ChannelR GET
|]

instance Yesod Tfoo

getHomeR :: Handler RepHtml
getHomeR = do
  tfoo <- getYesod
  defaultLayout [whamlet|
    <p>
      Start a new Take Five game with a human opponent
    <form method=post action=@{GamesR}>
      <input type=submit value="NEW GAME">
  |]

postGamesR :: Handler RepHtml
postGamesR = do
    tfoo <- getYesod
    id   <- liftIO $ newGameId tfoo
    redirect $ GameR id
  where newGameId tfoo = modifyMVar (nextGameId tfoo) incrementMVar
        incrementMVar value = return (value+1, value)

-- todo: refactor
updateGame :: Int -> Game -> Handler ()
updateGame id game = do
  tfoo <- getYesod
  liftIO $ modifyMVar (games tfoo) (\games ->
      return (Takefive.replace id (return game) games, games)
    )
  return ()

joinGame :: Int -> Mark -> Handler ()
joinGame id mark =
  do
    game <- getGame id
    setSession "player" $ T.pack playerId
    updateGame id $ setPlayer game mark playerId
    return ()
  where
    updatePlayer (o,x) = if mark == O then (playerId, x) else (o, playerId)
    playerId = (show id) ++ (show mark)

getGameR :: Int -> Handler RepHtml
getGameR id = do
  game <- getGame id
  maybePlayer <- lookupSession "player"
  defaultLayout [whamlet|
    Hi there
    <div>
      Player one:
      #{show $ playerO game}
    <div>
      Player two:
      #{show $ playerX game}
  |]

postPlayerOR :: Int -> Handler ()
postPlayerOR id = do
    return ()

postPlayerXR :: Int -> Handler ()
postPlayerXR id = do
    return ()

getChannelR :: Int -> Handler ()
getChannelR id = do
    game <- getGame id
    chan <- liftIO $ dupChan $ channel game
    req  <- waiRequest
    res  <- lift $ eventSourceApp chan req
    sendWaiResponse res

getGame :: Int -> Handler Game
getGame id = do
  tfoo <- getYesod
  maxId <- liftIO $ readMVar $ nextGameId tfoo
  list  <- liftIO $ readMVar $ games tfoo
  if id < maxId
    then (liftIO $ (list) !! id) >>= (\game -> return game)
    else notFound


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
  games <- newMVar gameStream
  warpDebug 3000 (Tfoo games nextGameId)

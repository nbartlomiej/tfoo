{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod
import Takefive as Takefive
import Control.Concurrent.Chan
import Data.Text as T
import Data.List as L
import Data.Maybe as M
import Control.Concurrent.MVar as V
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)

data Game = Game {
  players :: (MVar String, MVar String),
  channel :: Chan ServerEvent,
  board   :: MVar Takefive.Board
}

data Tfoo = Tfoo {
    games      :: MVar [IO Game],
    nextGameId :: MVar Int
  }

mkYesod "Tfoo" [parseRoutes|
/               HomeR GET
/games          GamesR POST
/games/#Int     GameR GET
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
  id <- liftIO $ newGameId tfoo
  joinGame id Takefive.O
  redirect $ GameR id

newGameId :: Tfoo -> IO Int
newGameId tfoo =
  modifyMVar (nextGameId tfoo) (\value -> return (value+1, value))

updateGame :: Int -> Game -> Handler ()
updateGame id game = do
  tfoo <- getYesod
  liftIO $ modifyMVar (games tfoo) (\games ->
      return (Takefive.replace id (return game) games, games)
    )
  return ()

joinGame :: Int -> Takefive.Mark -> Handler ()
joinGame id mark =
  let accessor = if mark == Takefive.O then fst else snd
      playerId = (show id) ++ (show mark)
  in do
    game <- getGame id
    setSession "player" $ T.pack playerId
    liftIO $ modifyMVar (accessor $ players game) (\v -> return (playerId, v))
    updateGame id game
    return ()

getGameR :: Int -> Handler RepHtml
getGameR id = do
  game   <- getGame id
  player <- lookupSession "player"
  o <- liftIO $ readMVar $ fst $ players game
  -- if Just o /= (player >>= (\p -> return $ T.unpack p) )
  --   then joinGame id Takefive.X
  --   else return ()
  defaultLayout [whamlet|
    Hi there
    #{o}
  |]

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
  playerOne <- newMVar "initial player one"
  playerTwo <- newMVar "initial player two"
  board     <- newMVar $ Takefive.generateBoard 20
  channel <- newChan
  return Game {
    players = (playerOne, playerTwo),
    channel = channel,
    board   = board
  }

gameStream :: [IO Game]
gameStream = repeat createGame

main :: IO ()
main = do
  nextGameId <- newMVar 1
  games <- newMVar gameStream
  warpDebug 3000 (Tfoo games nextGameId)

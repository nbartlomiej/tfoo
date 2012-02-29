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
    games      :: [IO Game],
    nextGameId :: (MVar Int)
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
  id <- liftIO $ createGame tfoo
  redirect $ GameR id

createGame :: Tfoo -> IO Int
createGame tfoo =
  modifyMVar (nextGameId tfoo) (\value -> return (value+1, value))

getGameR :: Int -> Handler RepHtml
getGameR id = do
  defaultLayout [whamlet| Hi there|]

gameStream :: [IO Game]
gameStream = L.map (\id -> do
      playerOne <- newEmptyMVar
      playerTwo <- newEmptyMVar
      board     <- newMVar $ Takefive.generateBoard 20
      channel <- newChan
      return Game {
        players = (playerOne, playerTwo),
        channel = channel,
        board   = board
      }
    ) [1..]

main :: IO ()
main = do
  nextGameId <- newMVar 1
  warpDebug 3000 (Tfoo gameStream nextGameId)

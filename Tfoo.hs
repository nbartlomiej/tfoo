{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
module Tfoo where

-- General functions that help operating on game data.
import Helpers

-- Specific functions implementing Take Five game logic.
import GameLogic

-- Core Haskell modules, mostly data types.
import Data.Text as T
import Data.List as L
import Data.Maybe as M
import Data.Monoid
import System.Random as Random
import Control.Monad

-- Monads for reading / setting memory state.
import Control.Concurrent.MVar

-- Yesod (web framework) modules.
import Yesod
import Yesod.Static
import Text.Hamlet (hamletFile)
import Text.Lucius (luciusFile)

-- Modules for creating and broadcasting to event source channel.
import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)
import Control.Concurrent.Chan
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

-- Reading commandline parameters
import System.Environment (getArgs)

type Player = String
data Game = Game {
  playerX :: Maybe Player,
  playerO :: Maybe Player,
  channel :: Chan ServerEvent,
  board   :: Board
}

setPlayer :: Game -> Mark -> Player -> Game
setPlayer game O playerId = game { playerO = Just playerId }
setPlayer game X playerId = game { playerX = Just playerId }

whoseTurn :: Game -> Maybe Player
whoseTurn g = if nextMark (board g) == O then playerO g else playerX g

data Tfoo = Tfoo {
    seed       :: Int,
    games      :: MVar [IO Game],
    nextGameId :: MVar Int,
    tfooStatic :: Static
  }

mkYesod "Tfoo" [parseRoutes|
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
    hamletToRepHtml $(hamletFile "templates/layout.hamlet")

getHomeR :: Handler RepHtml
getHomeR = do
  tfoo <- getYesod
  defaultLayout $ addHamlet $(hamletFile "templates/index.hamlet")

postGamesR :: Handler RepHtml
postGamesR = do
    tfoo <- getYesod
    id   <- liftIO $ newGameId tfoo
    redirect $ GameR id
  where -- Increment Tfoo's Game counter and return id of the next new Game.
    newGameId :: Tfoo -> IO Int
    newGameId tfoo = modifyMVar (nextGameId tfoo) incrementMVar

    incrementMVar :: Int -> IO (Int, Int)
    incrementMVar value = return (value+1, value)

getGameR :: Int -> Handler RepHtml
getGameR id = let
    columns = [0..19]
    rows    = [0..19]
  in do
    game <- getGame id
    maybePlayers <- lookupSession "players"
    tfoo <- getYesod
    defaultLayout $ do
      toWidgetHead [julius|
        var post = function(url){
          var xhr = new XMLHttpRequest();
          xhr.open("POST", url);
          xhr.send(null);
        };
        $(document).ready(function() {
          var src = new EventSource("@{ChannelR id}");
          src.onmessage = function(input) {
            var message = JSON.parse(input.data);
            if (message.id == "player-new"){
              $("#no_player_"+message.side).replaceWith("<div id='joined'>Joined</div>");
            } else if (message.id == "mark-new") {
              var markId = "#cell_"+message.x+"_"+message.y;
              $(markId).replaceWith(
                "<div id='"+markId+"' class='mark-"+message.mark+"'></div>"
              );
            } else if (message.id == "alert") {
              $("#messages").prepend(
                "<div class='message'>"+message.content+"</div>"
              );
            }
          };
          $('.mark-new').each(function(index, element){
            $(element).click(function(){
              var x = $(element).attr('data-x');
              var y = $(element).attr('data-y');
              post('#{show id}/mark/' + x + '/' + y);
            });
          });
        });
      |]
      addHamlet $(hamletFile "templates/game.hamlet")

postMarkR :: Int -> Int -> Int -> Handler ()
postMarkR id x y = do
    game               <- getGame id
    whoseTurn'         <- return $ whoseTurn game
    board'             <- return $ board game
    userAuthorizations <- do
      authorizations <- lookupSession "players"
      return $ fmap (L.words . T.unpack) authorizations

    -- The target cell has to be empty.
    require $ (getCell (board game) x y) == Nothing
    -- User has to be authorized to make this move
    require $ fromMaybe False (liftM2 elem whoseTurn' userAuthorizations)
    -- The game has to be still in progress
    require $ (winner board') == Nothing

    updateGame id $ game {board = replace' x y (Just $ nextMark board') board'}

    broadcast id "mark-new" [
        ("x", show x), ("y", show y), ("mark", show (nextMark board'))
      ]

    broadcastGameState id

  where require result = if result == False
          then permissionDenied "Permission Denied"
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

broadcastGameState :: Int -> Handler ()
broadcastGameState id = do
    game  <- getGame id
    board' <- return $ board game
    maybe (notifyNextPlayer board') announceWinner (winner board')
  where
    notifyNextPlayer board =
      broadcast id "alert" [("content", (show $ nextMark board)++"'s turn")]
    announceWinner mark =
      broadcast id "alert" [("content", "Game won: "++(show mark))]

joinGame :: Int -> Mark -> Handler ()
joinGame id mark =
  do
    game <- getGame id
    tfoo <- getYesod
    appendSession' "players" $ T.pack (playerId tfoo)
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
      return (Helpers.replace id (return game) games, games)
    )
  return ()

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

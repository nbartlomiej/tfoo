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
import Text.Hamlet (hamletFile)

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

joinGame :: Int -> Mark -> Handler ()
joinGame id mark =
  do
    game <- getGame id
    setSession "player" $ T.pack playerId
    updateGame id $ setPlayer game mark playerId
    return ()
  where
    playerId = (show id) ++ (show mark)

getGameR :: Int -> Handler RepHtml
getGameR id = do
  game <- getGame id
  maybePlayer <- lookupSession "player"
  defaultLayout $ do
    toWidgetHead [lucius|
      #channel {
        background: #ccc;
        width: 400px;
        height: 200px;
      }
    |]
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
    toWidgetHead [julius|
      var src = new EventSource("@{ChannelR id}");
      src.onmessage = function(input) {
          $("#channel").append(document.createTextNode(input.data));
          console.log(input.data);
          var message = JSON.parse(input.data);
          if (message.id == "player-joined"){
            $("#no_player_"+message.side).replaceWith("<div>Joined</div>");
          }

      };
      document.post = function(url){
        var xhr = new XMLHttpRequest();
        xhr.open("POST", url);
        xhr.send(null);
      };
    |]
    [whamlet|
      <div .players>
        <div #player_x>
          $maybe player <- (playerX game)
            <div #joined >
              Joined
              $maybe you <- maybePlayer
                $if (T.unpack you) == player
                  (You)
                $else
          $nothing
            <div #no_player_X >
              <form method=post action=@{PlayerXR id}>
                <input value="Join as X" type=submit>
        <div #player_o>
          $maybe player <- (playerO game)
            <div #joined >
              Joined
              $maybe you <- maybePlayer
                $if (T.unpack you) == player
                  (You)
                $else
          $nothing
            <div #no_player_O >
              <form method=post action=@{PlayerOR id}>
                <input value="Join as O" type=submit>
      <div #channel>
    |]


postPlayerOR :: Int -> Handler RepHtml
postPlayerOR id = do
  game <- getGame id
  if (playerO game) == Nothing
    then do
      joinGame id O
      broadcast id "player-joined" [("side", "O")]
      return ()
    else return ()
  redirect $ GameR id

postPlayerXR :: Int -> Handler RepHtml
postPlayerXR id = do
  game <- getGame id
  broadcast id "debug" [("message", "Invoked postPlayerXR")]
  if (playerX game) == Nothing
    then do
      joinGame id X
      broadcast id "player-joined" [("side", "X")]
      return ()
    else return ()
  redirect $ GameR id


type Category = String
broadcast :: Int -> String -> [(String, String)] -> Handler ()
broadcast gameId messageId pairs = do
  game <- getGame gameId
  liftIO $ writeChan (channel game) $ serverEvent $ return $ fromText message
  where message = T.pack $ "{"++(stringifiedPairs $ ("id",messageId):pairs)++"}"
        stringifiedPairs pairs = L.intercalate ", " $ L.map stringifyPair pairs
        stringifyPair p = "\""++(fst p) ++ "\": \"" ++ (snd p) ++ "\""
        serverEvent = ServerEvent Nothing Nothing

getChannelR :: Int -> Handler ()
getChannelR id = do
  game <- getGame id
  chan <- liftIO $ dupChan $ channel game
  req  <- waiRequest
  res  <- lift $ eventSourceApp chan req
  updateGame id game
  sendWaiResponse res

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
      return (Takefive.replace id (return game) games, games)
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
  games <- newMVar gameStream
  warpDebug 3000 (Tfoo games nextGameId)

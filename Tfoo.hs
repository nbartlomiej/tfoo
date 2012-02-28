{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod
import Control.Concurrent.Chan
import Data.Text as T
import Data.List as L
import Data.Maybe as M
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)

data Tfoo = Tfoo { channels :: [IO (Chan ServerEvent)] }

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
  redirect $ GameR 4

getGameR :: Int -> Handler RepHtml
getGameR id = do
  defaultLayout [whamlet| Hi there|]

lastFreeChannel :: [IO (Chan ServerEvent)] -> IO Int
lastFreeChannel (x:xs) = do
  channel <- x
  empty <- isEmptyChan channel
  if empty
    then return 0
    else do
      value <- lastFreeChannel xs
      return (value +1)

channelStream :: [IO (Chan ServerEvent)]
channelStream = do
  L.map (\channel -> newChan) [1..]

main :: IO ()
main = do
  warpDebug 3000 (Tfoo channelStream)

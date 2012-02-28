{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod
import Control.Concurrent.Chan
import Data.Text as T
import Data.List as L
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Network.Wai.EventSource (ServerEvent (..), eventSourceApp)

data Tfoo = Tfoo
  { channel :: [IO (Chan ServerEvent)] }

mkYesod "Tfoo" [parseRoutes|
/ HomeR GET
|]

instance Yesod Tfoo

getHomeR :: Handler RepHtml
getHomeR = do
  tfoo <- getYesod
  defaultLayout [whamlet| Empty |]

channels :: [IO (Chan ServerEvent)]
channels = do
  L.map (\channel -> newChan) [1..]

main :: IO ()
main = do
  warpDebug 3000 (Tfoo $ channels)

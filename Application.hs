{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

module Application where

import Tfoo.Foundation

import Yesod
import Yesod.Static

import Text.Hamlet (hamletFile)

mkYesodData "Tfoo" [parseRoutes|
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
      addScript $ StaticR $ StaticRoute ["scripts","jquery.wiggle.js"] []
    hamletToRepHtml $(hamletFile "Tfoo/Assets/Templates/layout.hamlet")



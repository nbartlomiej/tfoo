module Tfoo.Helpers.Application where

import Application
import Data.Text
import Data.Monoid
import Data.Maybe
import Yesod

-- Appends the given value to the session key.
appendSession :: Text -> Text -> Handler ()
appendSession name value = do
  initial <- lookupSession name
  setSession name $ fromJust $ initial `mappend` Just value

-- Appends the given value to the session key, inserts space before the value.
appendSession' :: Text -> Text -> Handler ()
appendSession' name value = appendSession name (pack " " `mappend` value)


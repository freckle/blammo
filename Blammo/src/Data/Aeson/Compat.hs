{-# LANGUAGE CPP #-}

module Data.Aeson.Compat
  ( Key
  , fromText
  , toText
  , KeyMap
  , empty
  , null
  , singleton
  , fromList
  , toList
  ) where

#if MIN_VERSION_aeson(2, 0, 0)
import Data.Aeson.Key (Key, fromText, toText)
import Data.Aeson.KeyMap (KeyMap, empty, fromList, null, singleton, toList)
-- Avoid unused-packages (unordered-containers) warning for this path
import Data.HashMap.Strict ()
#else
import Prelude (id)

import Data.HashMap.Strict (HashMap, empty, fromList, null, singleton, toList)
import Data.Text (Text)

type Key = Text
type KeyMap = HashMap Text

fromText :: Text -> Key
fromText = id

toText :: Key -> Text
toText = id
#endif

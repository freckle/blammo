module Blammo.Logging.Internal.Colors
  ( Colors (..)
  , colors
  , noColors
  , getColors
  ) where

import Prelude

import Data.Text (Text)

data Colors = Colors
  { gray :: Text -> Text
  , black :: Text -> Text
  , cyan :: Text -> Text
  , magenta :: Text -> Text
  , blue :: Text -> Text
  , yellow :: Text -> Text
  , green :: Text -> Text
  , red :: Text -> Text
  , bold :: Text -> Text
  , dim :: Text -> Text
  }

colors :: Colors
colors =
  Colors
    { gray = esc "0;37"
    , cyan = esc "0;36"
    , magenta = esc "0;35"
    , blue = esc "0;34"
    , yellow = esc "0;33"
    , green = esc "0;32"
    , red = esc "0;31"
    , black = esc "0;30"
    , bold = esc "1"
    , dim = esc "2"
    }
 where
  esc :: Text -> Text -> Text
  esc code x = "\ESC[" <> code <> "m" <> x <> "\ESC[0m"

noColors :: Colors
noColors =
  Colors
    { gray = id
    , black = id
    , cyan = id
    , magenta = id
    , blue = id
    , yellow = id
    , green = id
    , red = id
    , bold = id
    , dim = id
    }

getColors :: Bool -> Colors
getColors = \case
  True -> colors
  False -> noColors

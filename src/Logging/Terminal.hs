-- | Colorful logging for humans
--
-- Lines are formatted as
--
-- @
-- {timestamp} [{level}] {message} {details}
-- @
--
-- @level@ is padded to 9 characters and @message@ is padded to 31. This means
-- things will align as long as values are shorter than that. Longer values will
-- overflow (not be truncated). @details@ will show primitive values as
-- @key=value@, but ellides objects (as @key={...}@) and arrays (as
-- @key=[...]@).
--
-- This format was designed to match Python's
-- [structlog](https://www.structlog.org/en/stable/) package in its default
-- configuration.
--
module Logging.Terminal
  ( reformatTerminal
  ) where

import Prelude

import Control.Monad.Logger.Aeson
import Control.Monad.Logger.Aeson.Internal (KeyMap)
import qualified Control.Monad.Logger.Aeson.Internal as Aeson
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (defaultTimeLocale, formatTime)
import Logging.Colors

-- TODO: Control.Monad.Logger.Aeson.Internal exposes compatibility functions for
-- everything but this. If we want to support aeson-1.x, I think we'll need our
-- own CPP. (At which point, I would probably stop using the .Internal too).
import qualified Data.Aeson.Key as Aeson (toText)

reformatTerminal :: Bool -> LogLevel -> ByteString -> ByteString
reformatTerminal useColor logLevel bytes = fromMaybe bytes $ do
  LoggedMessage {..} <- decode $ BSL.fromStrict bytes

  let
    colors@Colors {..} = getColors useColor

    logTimestampText =
      pack $ formatTime defaultTimeLocale "%F %X" loggedMessageTimestamp

    logLevelText = case logLevel of
      LevelDebug -> lightgray $ padTo 9 "debug"
      LevelInfo -> green $ padTo 9 "info"
      LevelWarn -> yellow $ padTo 9 "warn"
      LevelError -> red $ padTo 9 "error"
      LevelOther x -> blue $ padTo 9 x

    loggedSourceAsMap = maybe
      Aeson.emptyKeyMap
      (\s -> Aeson.keyMapFromList [("source", String s)])
      loggedMessageLogSource

  pure $ encodeUtf8 $ mconcat
    [ logTimestampText <> " "
    , "[" <> logLevelText <> "] "
    , bold $ padTo 31 loggedMessageText
    , colorizeKeyMap colors loggedSourceAsMap
    , colorizeKeyMap colors loggedMessageThreadContext
    , colorizeKeyMap colors loggedMessageMeta
    ]

colorizeKeyMap :: Colors -> KeyMap Value -> Text
colorizeKeyMap Colors {..} km
  | km == Aeson.emptyKeyMap = ""
  | otherwise = " " <> T.intercalate " " keyValues
 where
  keyValues = map (uncurry fromKeyValue) $ Aeson.keyMapToList km

  fromKeyValue k v = cyan (Aeson.toText k) <> "=" <> magenta (fromValue v)

  fromValue :: Value -> Text
  fromValue = \case
    Object _ -> "{...}"
    Array _ -> "[...]"
    String x -> x
    Number n -> pack $ show n -- TODO: drop the ".0" from whole numbers
    Bool b -> pack $ show b
    Null -> "null"

padTo :: Int -> Text -> Text
padTo n t = t <> T.replicate pad " " where pad = max 0 $ n - T.length t

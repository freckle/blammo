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
-- overflow (not be truncated).
--
-- This format was designed to match Python's
-- [structlog](https://www.structlog.org/en/stable/) package in its default
-- configuration.
--
module Blammo.Logging.Terminal
  ( reformatTerminal
  ) where

import Prelude

import Blammo.Logging.Colors
import Control.Monad.Logger.Aeson
import Data.Aeson
import Data.Aeson.Compat (KeyMap)
import qualified Data.Aeson.Compat as Key
import qualified Data.Aeson.Compat as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (defaultTimeLocale, formatTime)
import qualified Data.Vector as V

reformatTerminal :: Bool -> LogLevel -> ByteString -> ByteString
reformatTerminal useColor logLevel bytes = fromMaybe bytes $ do
  LoggedMessage {..} <- decode $ BSL.fromStrict bytes

  let
    colors@Colors {..} = getColors useColor

    logTimestampText =
      dim $ pack $ formatTime defaultTimeLocale "%F %X" loggedMessageTimestamp

    logLevelText = case logLevel of
      LevelDebug -> gray $ padTo 9 "debug"
      LevelInfo -> green $ padTo 9 "info"
      LevelWarn -> yellow $ padTo 9 "warn"
      LevelError -> red $ padTo 9 "error"
      LevelOther x -> blue $ padTo 9 x

    loggedSourceAsMap =
      foldMap (KeyMap.singleton "source" . String) loggedMessageLogSource

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
  | KeyMap.null km = ""
  | otherwise = " " <> T.intercalate " " keyValues
 where
  keyValues = map (uncurry renderPair) $ KeyMap.toList km

  renderPair k v = cyan (Key.toText k) <> "=" <> magenta (fromValue v)

  fromValue = \case
    Object m -> obj $ map (uncurry renderPairNested) $ KeyMap.toList m
    Array a -> list $ map fromValue $ V.toList a
    String x -> x
    Number n -> sci n
    Bool b -> pack $ show b
    Null -> "null"

  renderPairNested k v = Key.toText k <> ": " <> fromValue v

  obj xs = "{" <> T.intercalate ", " xs <> "}"
  list xs = "[" <> T.intercalate ", " xs <> "]"
  sci = dropSuffix ".0" . pack . show

dropSuffix :: Text -> Text -> Text
dropSuffix suffix t = fromMaybe t $ T.stripSuffix suffix t

padTo :: Int -> Text -> Text
padTo n t = t <> T.replicate pad " " where pad = max 0 $ n - T.length t

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

-- TODO
import qualified Data.Aeson.Key as Aeson (toText)

reformatTerminal :: Bool -> LogLevel -> ByteString -> ByteString
reformatTerminal useColor logLevel bytes = fromMaybe bytes $ do
  LoggedMessage {..} <- decode $ BSL.fromStrict bytes

  let
    colors@Colors {..} = getColors useColor

    logLevelText = case logLevel of
      LevelDebug -> lightgray $ minimumWidth 9 "debug"
      LevelInfo -> green $ minimumWidth 9 "info"
      LevelWarn -> yellow $ minimumWidth 9 "warn"
      LevelError -> red $ minimumWidth 9 "error"
      LevelOther x -> blue $ minimumWidth 9 x

  pure $ encodeUtf8 $ mconcat
    [ black $ pack $ formatTime defaultTimeLocale "%F %X" loggedMessageTimestamp
    , " "
    , black "[" <> logLevelText <> black "]"
    , " "
    , bold $ minimumWidth 31 loggedMessageText
    , colorizeKeyMap colors $ maybe
      Aeson.emptyKeyMap
      (\s -> Aeson.keyMapFromList [("source", String s)])
      loggedMessageLogSource
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
    Number n -> pack $ show n
    Bool b -> pack $ show b
    Null -> "null"

minimumWidth :: Int -> Text -> Text
minimumWidth n t = t <> T.replicate pad " " where pad = max 0 $ n - T.length t

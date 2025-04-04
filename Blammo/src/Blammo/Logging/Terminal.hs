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
module Blammo.Logging.Terminal
  ( reformatTerminal
  ) where

import Prelude

import Blammo.Logging.Colors
import Blammo.Logging.LogSettings (LogSettings, getLogSettingsBreakpoint)
import Blammo.Logging.Terminal.LogPiece (LogPiece, logPiece)
import qualified Blammo.Logging.Terminal.LogPiece as LogPiece
import Control.Monad.Logger.Aeson
import Data.Aeson
import Data.Aeson.Compat (KeyMap)
import qualified Data.Aeson.Compat as Key
import qualified Data.Aeson.Compat as KeyMap
import Data.ByteString (ByteString)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import qualified Data.Vector as V

reformatTerminal
  :: LogSettings -> Colors -> LogLevel -> LoggedMessage -> ByteString
reformatTerminal settings colors@Colors {..} logLevel LoggedMessage {..} = do
  LogPiece.bytestring $
    if LogPiece.visibleLength oneLineLogPiece <= breakpoint
      then oneLineLogPiece
      else multiLineLogPiece
 where
  breakpoint = getLogSettingsBreakpoint settings

  logTimestampPiece =
    logPiece dim $
      pack $
        formatTime
          defaultTimeLocale
          "%F %X"
          loggedMessageTimestamp

  logLevelPiece = case logLevel of
    LevelDebug -> logPiece gray $ padTo 9 "debug"
    LevelInfo -> logPiece green $ padTo 9 "info"
    LevelWarn -> logPiece yellow $ padTo 9 "warn"
    LevelError -> logPiece red $ padTo 9 "error"
    LevelOther x -> logPiece blue $ padTo 9 x

  loggedSourceAsMap =
    foldMap (KeyMap.singleton "source" . String) loggedMessageLogSource

  logPrefixPiece =
    logTimestampPiece <> " [" <> logLevelPiece <> "] "

  logMessagePiece = logPiece bold $ padTo 31 loggedMessageText

  logAttrsPiece =
    mconcat
      [ colorizeKeyMap " " colors loggedSourceAsMap
      , colorizeKeyMap " " colors loggedMessageThreadContext
      , colorizeKeyMap " " colors loggedMessageMeta
      ]

  oneLineLogPiece = mconcat [logPrefixPiece, logMessagePiece, logAttrsPiece]

  multiLineLogPiece =
    let shift = "\n" <> LogPiece.offset (LogPiece.visibleLength logPrefixPiece)
    in  mconcat
          [ logPrefixPiece
          , logMessagePiece
          , colorizeKeyMap shift colors loggedSourceAsMap
          , colorizeKeyMap shift colors loggedMessageThreadContext
          , colorizeKeyMap shift colors loggedMessageMeta
          ]

colorizeKeyMap :: LogPiece -> Colors -> KeyMap Value -> LogPiece
colorizeKeyMap sep Colors {..} km
  | KeyMap.null km = mempty
  | otherwise = foldMap (uncurry fromPair) $ sortOn fst $ KeyMap.toList km
 where
  fromPair k v =
    sep <> logPiece cyan (Key.toText k) <> " = " <> logPiece magenta (fromValue v)

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

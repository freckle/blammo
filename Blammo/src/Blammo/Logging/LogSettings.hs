module Blammo.Logging.LogSettings
  ( LogSettings
  , LogLevels
  , LogDestination (..)
  , LogFormat (..)
  , LogColor (..)

    -- * Reading settings, e.g. from @ENV@
  , readLogLevels
  , readLogDestination
  , readLogFormat
  , readLogColor

    -- * Construction
  , defaultLogSettings

    -- * Modify
  , setLogSettingsLevels
  , setLogSettingsDestination
  , setLogSettingsFormat
  , setLogSettingsColor
  , setLogSettingsColors
  , setLogSettingsBreakpoint
  , setLogSettingsConcurrency

    -- * Access
  , getLogSettingsLevels
  , getLogSettingsDestination
  , getLogSettingsFormat
  , getLogSettingsColor
  , getLogSettingsBreakpoint
  , getLogSettingsConcurrency

    -- * Logic
  , adjustColors
  , shouldLogLevel
  , shouldColorAuto
  , shouldColorHandle
  ) where

import Prelude

import Blammo.Logging.Internal.Colors (Colors)
import Blammo.Logging.LogSettings.LogLevels (LogLevels)
import qualified Blammo.Logging.LogSettings.LogLevels as LogLevels
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger.Aeson
import System.IO (Handle, hIsTerminalDevice)
import qualified System.Info

data LogSettings = LogSettings
  { lsLevels :: LogLevels
  , lsDestination :: LogDestination
  , lsFormat :: LogFormat
  , lsColor :: LogColor
  , lsColors :: Colors -> Colors
  , lsBreakpoint :: Int
  , lsConcurrency :: Maybe Int
  }

readLogLevels :: String -> Either String LogLevels
readLogLevels = LogLevels.readLogLevels

data LogDestination
  = LogDestinationStdout
  | LogDestinationStderr
  | LogDestinationFile FilePath

readLogDestination :: String -> Either String LogDestination
readLogDestination = \case
  "stdout" -> Right LogDestinationStdout
  "stderr" -> Right LogDestinationStderr
  "null" -> Right $ LogDestinationFile nullDevice
  ('@' : path) -> Right $ LogDestinationFile path
  x ->
    Left $
      "Invalid log destination "
        <> x
        <> ", must be stdout, stderr, null, or @{path}"

data LogFormat
  = LogFormatJSON
  | LogFormatTerminal

readLogFormat :: String -> Either String LogFormat
readLogFormat = \case
  "tty" -> Right LogFormatTerminal
  "json" -> Right LogFormatJSON
  x -> Left $ "Invalid log format " <> x <> ", must be tty or json"

data LogColor
  = LogColorAuto
  | LogColorAlways
  | LogColorNever
  deriving stock (Eq, Show)

readLogColor :: String -> Either String LogColor
readLogColor x
  | x `elem` autoValues =
      Right LogColorAuto
  | x `elem` alwaysValues =
      Right LogColorAlways
  | x `elem` neverValues =
      Right LogColorNever
  | otherwise =
      Left $ "Invalid log color " <> x <> ", must be auto, always, or never"
 where
  autoValues :: [String]
  autoValues = ["auto"]

  alwaysValues :: [String]
  alwaysValues = ["always", "on", "yes", "true"]

  neverValues :: [String]
  neverValues = ["never", "off", "no", "false"]

defaultLogSettings :: LogSettings
defaultLogSettings =
  LogSettings
    { lsLevels = LogLevels.defaultLogLevels
    , lsDestination = LogDestinationStdout
    , lsFormat = LogFormatTerminal
    , lsColor = LogColorAuto
    , lsColors = id
    , lsBreakpoint = 120
    , lsConcurrency = Just 1
    }

setLogSettingsLevels :: LogLevels -> LogSettings -> LogSettings
setLogSettingsLevels x ls = ls {lsLevels = x}

setLogSettingsDestination :: LogDestination -> LogSettings -> LogSettings
setLogSettingsDestination x ls = ls {lsDestination = x}

setLogSettingsFormat :: LogFormat -> LogSettings -> LogSettings
setLogSettingsFormat x ls = case x of
  LogFormatTerminal ->
    ls
      { lsFormat = x
      , lsConcurrency = Just 1
      }
  _ ->
    ls
      { lsFormat = x
      , lsConcurrency = Nothing
      }

setLogSettingsColor :: LogColor -> LogSettings -> LogSettings
setLogSettingsColor x ls = ls {lsColor = x}

setLogSettingsBreakpoint :: Int -> LogSettings -> LogSettings
setLogSettingsBreakpoint x ls = ls {lsBreakpoint = x}

-- | Set the number of 'LoggerSet' Buffers used by @fast-logger@
--
-- A value of 'Nothing' means to use 'getNumCapabilities'. Higher is more
-- performant, but may deliver messages out of order. The defualt is set for TTY
-- usage (so, @1@), but is also changed through 'setLogSettingsFormat' is used.
--
-- Support for this option depends on your version of @fast-logger@:
--
-- +-----------------------------+------------+
-- | fast-logger | Destination   | Supported? |
-- +=============+===============+============+
-- | >=3.1.1     | anywhere      | yes        |
-- +-----------------------------+------------+
-- | >=3.0.5     | file          | yes        |
-- +-----------------------------+------------+
-- | >=3.0.5     | stdout/stderr | no         |
-- +-----------------------------+------------+
-- |  <3.0.5     | anywhere      | no         |
-- +-----------------------------+------------+
setLogSettingsConcurrency :: Maybe Int -> LogSettings -> LogSettings
setLogSettingsConcurrency x ls = ls {lsConcurrency = x}

-- | Set a function to modify 'Colors' used in logging
setLogSettingsColors :: (Colors -> Colors) -> LogSettings -> LogSettings
setLogSettingsColors f ls = ls {lsColors = f}

getLogSettingsLevels :: LogSettings -> LogLevels
getLogSettingsLevels = lsLevels

getLogSettingsDestination :: LogSettings -> LogDestination
getLogSettingsDestination = lsDestination

getLogSettingsFormat :: LogSettings -> LogFormat
getLogSettingsFormat = lsFormat

getLogSettingsColor :: LogSettings -> LogColor
getLogSettingsColor = lsColor

getLogSettingsBreakpoint :: LogSettings -> Int
getLogSettingsBreakpoint = lsBreakpoint

getLogSettingsConcurrency :: LogSettings -> Maybe Int
getLogSettingsConcurrency = lsConcurrency

adjustColors :: LogSettings -> Colors -> Colors
adjustColors = lsColors

shouldLogLevel :: LogSettings -> LogSource -> LogLevel -> Bool
shouldLogLevel = LogLevels.shouldLogLevel . getLogSettingsLevels

shouldColorAuto :: Applicative m => LogSettings -> m Bool -> m Bool
shouldColorAuto LogSettings {..} f = case lsColor of
  LogColorAuto -> f
  LogColorAlways -> pure True
  LogColorNever -> pure False

shouldColorHandle :: MonadIO m => LogSettings -> Handle -> m Bool
shouldColorHandle settings h =
  shouldColorAuto settings $ liftIO $ hIsTerminalDevice h

-- | @/dev/null@ or @NUL@ on windows
--
-- See <https://stackoverflow.com/a/58177337>.
nullDevice :: FilePath
nullDevice
  | System.Info.os == windowsOS = "\\\\.\\NUL"
  | otherwise = "/dev/null"

windowsOS :: String
windowsOS = "mingw32"

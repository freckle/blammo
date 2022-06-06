module Blammo.Logging.LogSettings
  ( LogSettings
  , LogLevels
  , LogDestination(..)
  , LogFormat(..)
  , LogColor(..)

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

  -- * Access
  , getLogSettingsLevels
  , getLogSettingsDestination
  , getLogSettingsFormat
  , getLogSettingsColor

  -- * Logic
  , shouldLogLevel
  , shouldColorAuto
  , shouldColorHandle
  ) where

import Prelude

import Blammo.Logging.LogSettings.LogLevels (LogLevels)
import qualified Blammo.Logging.LogSettings.LogLevels as LogLevels
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger.Aeson
import System.IO (Handle, hIsTerminalDevice)

data LogSettings = LogSettings
  { lsLevels :: LogLevels
  , lsDestination :: LogDestination
  , lsFormat :: LogFormat
  , lsColor :: LogColor
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
  ('@' : path) -> Right $ LogDestinationFile path
  x ->
    Left
      $ "Invalid log destination "
      <> x
      <> ", must be stdout, stderr, or @{path}"

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

readLogColor :: String -> Either String LogColor
readLogColor x
  | x `elem` autoValues
  = Right LogColorAuto
  | x `elem` alwaysValues
  = Right LogColorAlways
  | x `elem` neverValues
  = Right LogColorNever
  | otherwise
  = Left $ "Invalid log color " <> x <> ", must be auto, always, or never"
 where
  autoValues :: [String]
  autoValues = ["auto"]

  alwaysValues :: [String]
  alwaysValues = ["always", "on", "yes", "true"]

  neverValues :: [String]
  neverValues = ["never", "off", "no", "false"]

defaultLogSettings :: LogSettings
defaultLogSettings = LogSettings
  { lsLevels = LogLevels.defaultLogLevels
  , lsDestination = LogDestinationStdout
  , lsFormat = LogFormatTerminal
  , lsColor = LogColorAuto
  }

setLogSettingsLevels :: LogLevels -> LogSettings -> LogSettings
setLogSettingsLevels x ls = ls { lsLevels = x }

setLogSettingsDestination :: LogDestination -> LogSettings -> LogSettings
setLogSettingsDestination x ls = ls { lsDestination = x }

setLogSettingsFormat :: LogFormat -> LogSettings -> LogSettings
setLogSettingsFormat x ls = ls { lsFormat = x }

setLogSettingsColor :: LogColor -> LogSettings -> LogSettings
setLogSettingsColor x ls = ls { lsColor = x }

getLogSettingsLevels :: LogSettings -> LogLevels
getLogSettingsLevels = lsLevels

getLogSettingsDestination :: LogSettings -> LogDestination
getLogSettingsDestination = lsDestination

getLogSettingsFormat :: LogSettings -> LogFormat
getLogSettingsFormat = lsFormat

getLogSettingsColor :: LogSettings -> LogColor
getLogSettingsColor = lsColor

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

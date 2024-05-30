module Blammo.Logging.Internal.LoggerLogAction (loggerLogAction) where

import Prelude

import Blammo.Logging.Internal.LogAction
import Blammo.Logging.Logger
import Control.Monad.Logger.Aeson hiding (LoggingT (..), filterLogger)

loggerLogAction :: Logger -> LogAction IO
loggerLogAction logger =
  LogAction $
    defaultOutputWith $
      defaultOutputOptions $
        \logLevel bytes ->
          pushLogStrLn logger $ toLogStr $ getLoggerReformat logger logLevel bytes

module Blammo.Logging.Internal.LoggerLogAction (loggerLogAction) where

import Prelude

import Blammo.Logging.Internal.LogAction
import Blammo.Logging.Internal.Logger (Logger (..))
import Blammo.Logging.Logger
import Control.Monad (when)
import Control.Monad.Logger.Aeson hiding (LoggingT (..), filterLogger)

loggerLogAction :: Logger -> LogAction IO
loggerLogAction logger =
  LogAction $ \loc source level msg ->
    when (lShouldLog logger source level) $
      defaultOutputWith options loc source level msg
 where
  options = defaultOutputOptions $
    \logLevel bytes ->
      pushLogStrLn logger $ toLogStr $ getLoggerReformat logger logLevel bytes

module Blammo.Logging.Internal.LogAction (LogAction (..), runLogAction) where

import Control.Monad.Logger.Aeson hiding (LoggingT (..))

newtype LogAction m = LogAction (Loc -> LogSource -> LogLevel -> LogStr -> m ())

runLogAction
  :: ToLogStr msg => LogAction m -> Loc -> LogSource -> LogLevel -> msg -> m ()
runLogAction (LogAction f) loc source level msg = f loc source level (toLogStr msg)

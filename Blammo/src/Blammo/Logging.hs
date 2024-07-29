module Blammo.Logging
  ( LogLevel (..)
  , Logger
  , HasLogger (..)
  , withLogger
  , newLogger
  , runLoggerLoggingT

    -- * Re-exports from "Control.Monad.Logger.Aeson"

    -- ** Messages
  , Message (..)
  , (.=)
  , Series

    -- ** Transformers
  , MonadLogger (..)
  , MonadLoggerIO (..)
  , LoggingT
  , WithLogger (..)
  , runWithLogger

    -- ** Common logging functions

    -- | Import "Control.Monad.Logger.Aeson" if you want more

    -- *** Implicit call stack, no 'LogSource'
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logOther

    -- *** Implicit call stack, with 'LogSource'
  , LogSource
  , logDebugNS
  , logInfoNS
  , logWarnNS
  , logErrorNS
  , logOtherNS
  ) where

import Blammo.Logging.Logger
import Blammo.Logging.WithLogger
import Control.Lens (view)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger.Aeson
import Data.Aeson (Series)
import UnliftIO.Exception (finally)

-- | Initialize logging, pass a 'Logger' to the callback, and clean up at the end
--
-- Applications should avoid calling this more than once in their lifecycle.
runLoggerLoggingT
  :: (MonadUnliftIO m, HasLogger env) => env -> LoggingT m a -> m a
runLoggerLoggingT env f =
  runLoggingT f (runLogAction logger) `finally` flushLogStr logger
 where
  logger = view loggerL env

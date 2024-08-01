module Blammo.Logging.Setup
  ( HasLogger (..)
  , withLogger
  , newLogger
  , runLoggerLoggingT
  , LoggingT
  , WithLogger (..)
  , runWithLogger
  , newLoggerEnv
  , withLoggerEnv
  , runSimpleLoggingT
  ) where

import Prelude

import Blammo.Logging
import qualified Blammo.Logging.LogSettings.Env as Env
import Blammo.Logging.Logger
import Blammo.Logging.WithLogger
import Control.Lens (view)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger.Aeson
import UnliftIO.Exception (finally)

-- | Construct a 'Logger' configured via environment variables
newLoggerEnv :: MonadIO m => m Logger
newLoggerEnv = liftIO $ newLogger =<< Env.parse

-- | Initialize logging (configured via environment variables),
--   pass a 'Logger' to the callback, and clean up at the end
--
-- Applications should avoid calling this more than once in their lifecycle.
withLoggerEnv :: MonadUnliftIO m => (Logger -> m a) -> m a
withLoggerEnv f = liftIO Env.parse >>= \logger -> withLogger logger f

-- | Construct a 'Logger' configured via environment variables and use it
runSimpleLoggingT :: MonadUnliftIO m => LoggingT m a -> m a
runSimpleLoggingT f = do
  logger <- newLoggerEnv
  runLoggerLoggingT logger f

-- | Initialize logging, pass a 'Logger' to the callback, and clean up at the end
--
-- Applications should avoid calling this more than once in their lifecycle.
runLoggerLoggingT
  :: (MonadUnliftIO m, HasLogger env) => env -> LoggingT m a -> m a
runLoggerLoggingT env f =
  runLoggingT f (runLogAction logger) `finally` flushLogStr logger
 where
  logger = view loggerL env

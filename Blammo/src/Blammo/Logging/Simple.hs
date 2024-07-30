-- | Simplified out-of-the-box logging
module Blammo.Logging.Simple
  ( newLoggerEnv
  , withLoggerEnv
  , runSimpleLoggingT
  , module Blammo.Logging
  , module Blammo.Logging.LogSettings
  , module Blammo.Logging.ThreadContext
  ) where

import Prelude

import Blammo.Logging
import qualified Blammo.Logging.LogSettings.Env as Env
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Blammo.Logging.LogSettings
import Blammo.Logging.ThreadContext

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

-- | Simplified out-of-the-box logging
module Blammo.Logging.Simple
  ( newLoggerEnv
  , runSimpleLoggingT
  , module Blammo.Logging
  ) where

import Prelude

import Blammo.Logging
import qualified Blammo.Logging.LogSettings.Env as Env
import Control.Monad.IO.Class (MonadIO(..))

-- | Construct a 'Logger' configured via environment variables
newLoggerEnv :: MonadIO m => m Logger
newLoggerEnv = liftIO $ newLogger =<< Env.parse

-- | Construct a 'Logger' configured via environment variables and use it
runSimpleLoggingT :: MonadIO m => LoggingT m a -> m a
runSimpleLoggingT f = do
  logger <- newLoggerEnv
  runLoggerLoggingT logger f

-- | Simplified out-of-the-box logging
module Logging.Simple
  ( newLoggerEnv
  , runSimpleLoggingT
  , module Logging
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Logging
import qualified Logging.LogSettings.Env as Env

-- | Construct a 'Logger' configured via environment variables
newLoggerEnv :: MonadIO m => m Logger
newLoggerEnv = liftIO $ newLogger =<< Env.parse

-- | Construct a 'Logger' configured via environment variables and use it
runSimpleLoggingT :: MonadIO m => LoggingT m a -> m a
runSimpleLoggingT f = do
  logger <- newLoggerEnv
  runLoggerLoggingT logger f

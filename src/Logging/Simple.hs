module Logging.Simple
  ( newLoggerEnv
  , runSimpleLoggingT
  , module Logging
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Logging
import qualified Logging.Settings.Env as Env

newLoggerEnv :: MonadIO m => m Logger
newLoggerEnv = liftIO $ newLogger =<< Env.parse

runSimpleLoggingT :: MonadIO m => LoggingT m a -> m a
runSimpleLoggingT f = do
  logger <- newLoggerEnv
  runLoggerLoggingT logger f

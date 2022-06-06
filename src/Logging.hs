module Logging
  ( LogSettings
  , LogLevel(..)
  , LogDestination(..)
  , LogFormat(..)
  , LogColor(..)
  , defaultLogSettings
  , setLogSettingsLevels
  , setLogSettingsDestination
  , setLogSettingsFormat
  , setLogSettingsColor
  , Logger
  , HasLogger(..)
  , newLogger
  , runLoggerLoggingT

  -- * Re-exports from "Control.Monad.Logger.Aeson"
  -- ** Messages
  , Message(..)
  , (.=)
  , Series

  -- ** Thread Context
  , MonadMask
  , withThreadContext
  , myThreadContext
  , Pair

  -- ** Transformer
  , MonadLogger(..)
  , MonadLoggerIO(..)
  , LoggingT

  -- ** Common logging functions
  -- | Import "Control.Monad.Logger.Aeson" if you want more

  -- *** Implicit call stack, no 'LogSource'
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logOther

  -- *** Implicit call stack, with 'LogSource'
  , logDebugNS
  , logInfoNS
  , logWarnNS
  , logErrorNS
  , logOtherNS
  ) where

import Prelude

import Control.Lens ((^.))
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger.Aeson
import Data.Aeson (Series)
import Data.Aeson.Types (Pair)
import Data.ByteString (ByteString)
import Logging.LogSettings
import Logging.Logger
import System.Log.FastLogger (LoggerSet, flushLogStr, pushLogStrLn)

runLoggerLoggingT :: (MonadIO m, HasLogger env) => env -> LoggingT m a -> m a
runLoggerLoggingT env f = do
  a <- runLoggingT
    (filterLogger (getLoggerShouldLog logger) f)
    (loggerOutput loggerSet $ getLoggerReformat logger)
  a <$ liftIO (flushLogStr loggerSet)
 where
  logger = env ^. loggerL
  loggerSet = getLoggerLoggerSet logger

loggerOutput
  :: LoggerSet
  -> (LogLevel -> ByteString -> ByteString)
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
loggerOutput loggerSet reformat =
  defaultOutputWith $ defaultOutputOptions $ \logLevel bytes -> do
    pushLogStrLn loggerSet $ toLogStr $ reformat logLevel bytes

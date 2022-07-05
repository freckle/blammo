module Blammo.Logging.Logger
  ( Logger
  , HasLogger(..)
  , newLogger
  , getLoggerReformat
  , getLoggerShouldLog
  , pushLogStrLn
  , flushLogStr

  -- * Testing
  , newTestLogger
  , LoggedMessage(..)
  , getLoggedMessages
  , getLoggedMessagesLenient
  , getLoggedMessagesUnsafe

  -- * Deprecated
  , getLoggerLoggerSet
  ) where

import Prelude

import Blammo.Logging.LogSettings
import Blammo.Logging.Terminal
import Blammo.Logging.Test hiding (getLoggedMessages)
import qualified Blammo.Logging.Test as LoggedMessages
import Control.Lens (Lens', view)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger.Aeson
import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import Data.Either (partitionEithers, rights)
import Data.List (intercalate)
import System.IO (stderr, stdout)
import System.Log.FastLogger
  ( LoggerSet
  , defaultBufSize
  , newFileLoggerSet
  , newStderrLoggerSet
  , newStdoutLoggerSet
  )
import qualified System.Log.FastLogger as FastLogger (flushLogStr, pushLogStrLn)
import UnliftIO.Exception (throwString)

data Logger = Logger
  { lLoggerSet :: LoggerSet
  , lReformat :: LogLevel -> ByteString -> ByteString
  , lShouldLog :: LogSource -> LogLevel -> Bool
  , lLoggedMessages :: Maybe LoggedMessages
  }

getLoggerLoggerSet :: Logger -> LoggerSet
getLoggerLoggerSet = lLoggerSet
{-# DEPRECATED getLoggerLoggerSet "Internal function, will be removed in a future version" #-}

getLoggerReformat :: Logger -> LogLevel -> ByteString -> ByteString
getLoggerReformat = lReformat

getLoggerShouldLog :: Logger -> LogSource -> LogLevel -> Bool
getLoggerShouldLog = lShouldLog

pushLogStrLn :: MonadIO m => Logger -> LogStr -> m ()
pushLogStrLn logger str = case lLoggedMessages logger of
  Nothing -> liftIO $ FastLogger.pushLogStrLn loggerSet str
  Just lm -> appendLogStr lm str
  where loggerSet = getLoggerLoggerSet logger

flushLogStr :: MonadIO m => Logger -> m ()
flushLogStr logger = case lLoggedMessages logger of
  Nothing -> liftIO $ FastLogger.flushLogStr loggerSet
  Just _ -> pure ()
  where loggerSet = getLoggerLoggerSet logger

class HasLogger env where
    loggerL :: Lens' env Logger

instance HasLogger Logger where
  loggerL = id

newLogger :: MonadIO m => LogSettings -> m Logger
newLogger settings = do
  (lLoggerSet, useColor) <- liftIO $ case getLogSettingsDestination settings of
    LogDestinationStdout ->
      (,)
        <$> newStdoutLoggerSet defaultBufSize
        <*> shouldColorHandle settings stdout
    LogDestinationStderr ->
      (,)
        <$> newStderrLoggerSet defaultBufSize
        <*> shouldColorHandle settings stderr
    LogDestinationFile path ->
      (,) <$> newFileLoggerSet defaultBufSize path <*> shouldColorAuto
        settings
        (pure False)

  let
    lReformat = case getLogSettingsFormat settings of
      LogFormatJSON -> const id -- Color is ignored
      LogFormatTerminal -> reformatTerminal useColor

    lShouldLog = shouldLogLevel settings
    lLoggedMessages = Nothing

  pure $ Logger { .. }

-- | Create a 'Logger' that will capture log messages instead of logging them
--
-- See "Blammo.Logging.LoggedMessages" for more details.
--
newTestLogger :: MonadIO m => LogSettings -> m Logger
newTestLogger settings = go <$> newLogger settings <*> newLoggedMessages
 where
  go logger loggedMessages =
    logger { lReformat = const id, lLoggedMessages = Just loggedMessages }

-- | Return the logged messages if 'newTestLogger' was used
--
-- If not, the empty list is returned.
--
getLoggedMessages
  :: (MonadIO m, MonadReader env m, HasLogger env)
  => m [Either String LoggedMessage]
getLoggedMessages = do
  logger <- view loggerL
  maybe (pure []) LoggedMessages.getLoggedMessages $ lLoggedMessages logger

-- | 'getLoggedMessages' but ignore any messages that fail to parse
getLoggedMessagesLenient
  :: (MonadIO m, MonadReader env m, HasLogger env) => m [LoggedMessage]
getLoggedMessagesLenient = rights <$> getLoggedMessages

-- | 'getLoggedMessages' but 'throwString' if any messages failed to parse
getLoggedMessagesUnsafe
  :: (MonadIO m, MonadReader env m, HasLogger env) => m [LoggedMessage]
getLoggedMessagesUnsafe = do
  (failed, succeeded) <- partitionEithers <$> getLoggedMessages

  succeeded <$ unless
    (null failed)
    (throwString
    $ intercalate "\n"
    $ "Messages were logged that didn't parse as LoggedMessage:"
    : failed
    )

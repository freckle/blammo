module Blammo.Logging.Logger
  ( Logger
  , HasLogger (..)
  , withLogger
  , newLogger
  , flushLogger
  , pushLogger
  , pushLoggerLn
  , getLoggerLogSettings
  , getLoggerReformat
  , setLoggerReformat
  , getLoggerShouldLog
  , getLoggerShouldColor
  , pushLogStrLn
  , flushLogStr
  , runLogAction

    -- * Testing
  , newTestLogger
  , LoggedMessage (..)
  , getLoggedMessages
  , getLoggedMessagesLenient
  , getLoggedMessagesUnsafe
  ) where

import Prelude

import Blammo.Logging.Colors (Colors, getColors)
import Blammo.Logging.Internal.Logger
import Blammo.Logging.LogSettings
import Blammo.Logging.Terminal
import Blammo.Logging.Test hiding (getLoggedMessages)
import qualified Blammo.Logging.Test as LoggedMessages
import Control.Lens (view)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger.Aeson
import Control.Monad.Reader (MonadReader)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Either (partitionEithers, rights)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import System.IO (stderr, stdout)
import System.Log.FastLogger (LoggerSet, defaultBufSize)
import qualified System.Log.FastLogger as FastLogger
  ( flushLogStr
  , pushLogStr
  , pushLogStrLn
  )
import System.Log.FastLogger.Compat
  ( newFileLoggerSetN
  , newStderrLoggerSetN
  , newStdoutLoggerSetN
  )
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (finally, throwString)

-- | Initialize logging, pass a 'Logger' to the callback, and clean up at the end
--
-- Applications should avoid calling this more than once in their lifecycle.
withLogger :: MonadUnliftIO m => LogSettings -> (Logger -> m a) -> m a
withLogger settings f = do
  logger <- newLogger settings
  f logger `finally` flushLogStr logger

-- | Write a message to the 'Logger', unless the logger's filter options
--   reject it based on its 'LogSource' and 'LogLevel'
runLogAction
  :: (MonadIO m, ToLogStr msg)
  => Logger
  -> Loc
  -> LogSource
  -> LogLevel
  -> msg
  -> m ()
runLogAction logger loc source level msg =
  liftIO $
    when (lShouldLog logger source level) $
      defaultOutputWith options loc source level (toLogStr msg)
 where
  options = defaultOutputOptions $ \logLevel bytes ->
    pushLogStrLn logger $ toLogStr $ getLoggerReformat logger logLevel bytes

getLoggerLogSettings :: Logger -> LogSettings
getLoggerLogSettings = lLogSettings

getLoggerLoggerSet :: Logger -> LoggerSet
getLoggerLoggerSet = lLoggerSet

getLoggerReformat :: Logger -> LogLevel -> ByteString -> ByteString
getLoggerReformat = lReformat

setLoggerReformat
  :: (LogSettings -> Colors -> LogLevel -> LoggedMessage -> ByteString)
  -> Logger
  -> Logger
setLoggerReformat f logger =
  logger
    { lReformat = \level bytes -> fromMaybe bytes $ do
        lm <- Aeson.decodeStrict bytes
        pure $ f (lLogSettings logger) (getColors $ lShouldColor logger) level lm
    }

getLoggerShouldLog :: Logger -> LogSource -> LogLevel -> Bool
getLoggerShouldLog = lShouldLog

getLoggerShouldColor :: Logger -> Bool
getLoggerShouldColor = lShouldColor

pushLogStr :: MonadIO m => Logger -> LogStr -> m ()
pushLogStr logger str = case lLoggedMessages logger of
  Nothing -> liftIO $ FastLogger.pushLogStr loggerSet str
  Just lm -> appendLogStr lm str
 where
  loggerSet = getLoggerLoggerSet logger

pushLogStrLn :: MonadIO m => Logger -> LogStr -> m ()
pushLogStrLn logger str = case lLoggedMessages logger of
  Nothing -> liftIO $ FastLogger.pushLogStrLn loggerSet str
  Just lm -> appendLogStrLn lm str
 where
  loggerSet = getLoggerLoggerSet logger

flushLogStr :: MonadIO m => Logger -> m ()
flushLogStr logger = case lLoggedMessages logger of
  Nothing -> liftIO $ FastLogger.flushLogStr loggerSet
  Just _ -> pure ()
 where
  loggerSet = getLoggerLoggerSet logger

newLogger :: MonadIO m => LogSettings -> m Logger
newLogger settings = do
  (lLoggerSet, lShouldColor) <-
    liftIO $ case getLogSettingsDestination settings of
      LogDestinationStdout ->
        (,)
          <$> newStdoutLoggerSetN defaultBufSize concurrency
          <*> shouldColorHandle settings stdout
      LogDestinationStderr ->
        (,)
          <$> newStderrLoggerSetN defaultBufSize concurrency
          <*> shouldColorHandle settings stderr
      LogDestinationFile path ->
        (,)
          <$> newFileLoggerSetN defaultBufSize concurrency path
          <*> shouldColorAuto settings (pure False)

  let logger =
        Logger
          { lLogSettings = settings
          , lLoggerSet = lLoggerSet
          , lReformat = const id -- By default render (JSON) bytestring as-is
          , lShouldLog = shouldLogLevel settings
          , lShouldColor = lShouldColor
          , lLoggedMessages = Nothing
          }

  pure $ case getLogSettingsFormat settings of
    LogFormatJSON -> logger
    LogFormatTerminal -> setLoggerReformat reformatTerminal logger
 where
  concurrency = getLogSettingsConcurrency settings

flushLogger :: (MonadIO m, MonadReader env m, HasLogger env) => m ()
flushLogger = do
  logger <- view loggerL
  flushLogStr logger

pushLogger :: (MonadIO m, MonadReader env m, HasLogger env) => Text -> m ()
pushLogger msg = do
  logger <- view loggerL
  pushLogStr logger $ toLogStr msg

pushLoggerLn :: (MonadIO m, MonadReader env m, HasLogger env) => Text -> m ()
pushLoggerLn msg = do
  logger <- view loggerL
  pushLogStrLn logger $ toLogStr msg

-- | Create a 'Logger' that will capture log messages instead of logging them
--
-- See "Blammo.Logging.LoggedMessages" for more details.
newTestLogger :: MonadIO m => LogSettings -> m Logger
newTestLogger settings = go <$> newLogger settings <*> newLoggedMessages
 where
  go logger loggedMessages =
    logger {lReformat = const id, lLoggedMessages = Just loggedMessages}

-- | Return the logged messages if 'newTestLogger' was used
--
-- If not, the empty list is returned.
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
  :: (HasCallStack, MonadIO m, MonadReader env m, HasLogger env)
  => m [LoggedMessage]
getLoggedMessagesUnsafe = do
  (failed, succeeded) <- partitionEithers <$> getLoggedMessages

  succeeded
    <$ unless
      (null failed)
      ( throwString $
          intercalate "\n" $
            "Messages were logged that didn't parse as LoggedMessage:"
              : failed
      )

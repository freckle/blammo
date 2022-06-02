# Logging

A batteries-included Structured Logging toolkit for writing to a single logging
abstraction in CLI apps and production services in Haskell.

## Example

<!--
```haskell
module Main (module Main) where

import Prelude

import Text.Markdown.Unlit ()
```
-->

Minimal usage will only require the top-level library:

```haskell
import Data.Text (Text)
import Logging
```

```haskell
action :: MonadLogger m => m ()
action = do
  logInfo "This is a message sans details"
  logError $ "Something went wrong" :# ["error" .= ("oops" :: Text)]
  logDebug "This won't be seen in default settings"

runner1 :: LoggingT IO a -> IO a
runner1 f = do
  logger <- newLogger defaultLogSettings
  runLoggerLoggingT logger $ withThreadContext ["app" .= ("example" :: Text)] f
```

Running this basic example produces,

![](files/readme-terminal.png)

In production, you probably want to configure your structured logs for JSON, to
be ingested by some service.

```haskell
runner2 :: LoggingT IO a -> IO a
runner2 f = do
  logger <- newLogger
    . setLogSettingsFormat LogFormatJSON
    $ defaultLogSettings
  runLoggerLoggingT logger $ withThreadContext ["app" .= ("example" :: Text)] f
```

Running this example produces,

![](files/readme-json.png)

```haskell
main :: IO ()
main = do
  runner1 action
  runner2 action
```

## More Advanced Usage

## Environment-based Configuration

The `Logging.Settings` module exposes an `Either`-based reader function for each
field on the `LogSettings` type. These could be used manually:

```hs
eLogLevel <- readLogLevel <$> getEnv "LOG_LEVEL"
```

Or in something more involved, like [`envparse`][envparse]:

[envparse]: https://hackage.haskell.org/package/envparse

```hs
import Env

envLogSettings :: IO LogSettings
envLogSettings = parse id $ build
    <$> var (eitherReader readLogLevel) "LOG_LEVEL" (def LevelInfo)
    <*> var (eitherReader readLogDestination) "LOG_DESTINATION" (def LogDestinationStdout)
    <*> var (eitherReader readLogFormat) "LOG_FORMAT" (def LogFormatTerminal)
    <*> var (eitherReader readLogColor) "LOG_COLOR" (def LogColorAuto)
  where
    build level destination format color =
        setLogSettingsLevel level
            . setLogSettingsDestination destination
            . setLogSettingsFormat format
            . setLogSettingsColor color
            $ defaultLogSettings

eitherReader :: AsUnread e => (String -> Either String a) -> Reader e a
eitherReader f = first unread . f
```

## Integration with RIO

TODO

## Integration with Amazonka

```hs
data App = App
  { appLogger :: Logger
  , appAWS :: AWS.Env
  }

instance HasLogger App where
  -- ...

runApp :: ReaderT App (LoggingT IO) a -> IO a
runApp f = do
  logger <- newLogger defaultLogSettings
  app <- App logger <$> runLoggerLoggingT logger awsDiscover
  runLoggerLoggingT app $ runReaderT f app

awsDiscover :: (MonadIO m, MonadLoggerIO m) => m AWS.Env
awsDiscover = do
    monadLoggerLog <- askLoggerIO

    env <- liftIO $ AWS.newEnv AWS.discover
    pure $ env
        { AWS.envLogger = \level msg -> do
            monadLoggerLog
                defaultLoc
                "Amazonka"
                (fromLevel level)
                (toLogStr msg)
        }

fromLevel :: AWS.LogLevel -> LogLevel
fromLevel = \case
    AWS.Info -> LevelInfo
    AWS.Error -> LevelError
    AWS.Debug -> LevelDebug
    AWS.Trace -> LevelDebug
```

## Integration with WAI

```hs
import Network.Wai.Middleware.Logging

instance HasLogger App where
  -- ...

waiMiddleware :: App -> Middleware
waiMiddleware app = requestLogger app . defaultMiddlewaresNoLogging
```

## Integration with Warp

```hs
instance HasLogger App where
  -- ...

warpSettings :: App -> Settings
warpSettings app = setOnException onEx $ defaultSettings
  where
    onEx _req ex =
        when (defaultShouldDisplayException ex)
            $ runLoggerLoggingT app
            $ logError
            $ "Warp exception"
            :# ["exception" .= displayException ex]
```

## Integration with Yesod

```hs
import Logging.Logger (getLoggerLoggerSet)

instance HasLogger App where
 -- ...

instance Yesod App where
    -- ...
    makeLogger App {..} = do
        logger <- defaultMakeLogger
        pure $ logger { Y.loggerSet = getLoggerLoggerSet appLogger }

    messageLoggerSource app _logger loc source level msg =
        runLoggerLoggingT app $ monadLoggerLog loc source level msg
```

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)

# Logging

A batteries-included Structured Logging toolkit for writing to a single logging
abstraction in CLI apps and production services in Haskell.

## Simple Usage

<!--
```haskell
module Main (module Main) where

import Prelude

import Data.Aeson
import Data.Text (Text)
import Text.Markdown.Unlit ()
```
-->

```haskell
import Logging.Simple
```

Throughout your application, you should write against the ubiquitous
`MonadLogger` interface, but using [`monad-logger-aeson`][monad-logger-aeson]:

[monad-logger-aeson]: https://jship.github.io/posts/2022-05-17-announcing-monad-logger-aeson/

```haskell
action :: MonadLogger m => m ()
action = do
  logInfo "This is a message sans details"

  logError
    $ "Something went wrong"
    :# [ "error" .= object
          [ "code" .= (100 :: Int)
          , "messages" .= (["x", "y"] :: [Text])
          ]
       ]

  logDebug "This won't be seen in default settings"
```

When you run your transformer stack, wrap it in `runLoggerLoggingT` providing
any value with a `HasLogger` instance (such as your main `App`). The `Logger`
type itself has such an instance, and we provide `runSimpleLoggingT` for the
simplest case: it creates one configured via environment variables and then
calls `runLoggerLoggingT` with it.

You can use `withThreadContext` (from `monad-logger-aeson`) to add details that
will appear in all the logged messages within that scope. Placing one of these
at the very top-level adds details to all logged messages.

```haskell
runner :: LoggingT IO a -> IO a
runner = runSimpleLoggingT . withThreadContext ["app" .= ("example" :: Text)]

main :: IO ()
main = runner action
```

The defaults are good for CLI applications, producing colorful output (if
connected to a terminal device) suitable for a human:

![](files/readme-terminal.png)

Under the hood, `Logging.Settings.Env` is using [`envparse`][envparse] to
configure logging through environment variables. See that module for full
details. One thing we can adjust is `LOG_LEVEL`:

[envparse]: https://hackage.haskell.org/package/envparse

![](files/readme-terminal-debug.png)

In production, you will probably want to set `LOG_FORMAT=json` and ship logs to
some aggregator like Datadog or Mezmo (formerly LogDNA):

![](files/readme-json.png)

## Advanced Usage

TODO

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
        defaultLoc -- TODO: there may be a way to get a CallStack/Loc
        "Amazonka"
        (\case
            AWS.Info -> LevelInfo
            AWS.Error -> LevelError
            AWS.Debug -> LevelDebug
            AWS.Trace -> LevelDebug
        )
        (toLogStr msg)
    }
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

  messageLoggerSource app _logger loc source level msg =
    runLoggerLoggingT app $ monadLoggerLog loc source level msg
```

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)

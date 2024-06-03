# Blammo

[![Hackage](https://img.shields.io/hackage/v/Blammo.svg?style=flat)](https://hackage.haskell.org/package/Blammo)
[![Stackage Nightly](http://stackage.org/package/Blammo/badge/nightly)](http://stackage.org/nightly/package/Blammo)
[![Stackage LTS](http://stackage.org/package/Blammo/badge/lts)](http://stackage.org/lts/package/Blammo)
[![CI](https://github.com/freckle/blammo/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/blammo/actions/workflows/ci.yml)

![](files/blammo.png)

Blammo is a Structured Logging library that's

- Easy to use: one import and go!
- Easy to configure: environment variable parsing out of the box!
- Easy to integrate: see below for Amazonka, Yesod, and more!
- Produces beautiful, colorful output in development
- Produces fast-fast JSON in production

All built on the well-known `MonadLogger` interface and using an efficient
`fast-logger` implementation.

> It's better than bad, it's good!

## Simple Usage

<!--
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (module Main) where

import Prelude

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Markdown.Unlit ()
import Control.Lens (lens)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT))
```
-->

```haskell
import Blammo.Logging.Simple
```

Throughout your application, you should write against the ubiquitous
`MonadLogger` interface:

```haskell
action1 :: MonadLogger m => m ()
action1 = do
  logInfo "This is a message sans details"
```

And make use of [`monad-logger-aeson`][monad-logger-aeson] for structured
details:

[monad-logger-aeson]: https://jship.github.io/posts/2022-05-17-announcing-monad-logger-aeson/

```haskell
data MyError = MyError
  { code :: Int
  , messages :: [Text]
  }
  deriving stock Generic
  deriving anyclass ToJSON

action2 :: MonadLogger m => m ()
action2 = do
  logError $ "Something went wrong" :# ["error" .= MyError 100 ["x", "y"]]
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
main = runner $ do
  action1
  action2
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

## Multiline Format

With the terminal formatter, a log message that is more than 120 visible
characters will break into multi-line format:

![](files/multiline.png)

This breakpoint can be controlled with `LOG_BREAKPOINT`. Set an unreasonably
large number to disable this feature.

## Out of Order Messages

Blammo is built on [fast-logger], which offers concurrent logging through
multiple buffers. This concurrent logging is fast, but may deliver messages out
of order. You want this on production: your aggregator should be inspecting the
message's time-stamp to re-order as necessary on the other side. However, this
can be problematic in a CLI, where there is both little need for such high
performance and a lower tolerance for the confusion of out of order messages.

For this reason, the default behavior is to _not_ use concurrent logging, but
setting the format to `json` will automatically enable it (with
{number-of-cores} as the value). To handle this explicitly, set
`LOG_CONCURRENCY`.

[fast-logger]: https://hackage.haskell.org/package/fast-logger

## Configuration

| Setting     | Setter                      | Environment variable and format           |
| ----------- | --------------------------- | ----------------------------------------- |
| Format      | `setLogSettingsFormat`      | `LOG_FORMAT=tty\|json`                    |
| Level(s)    | `setLogSettingsLevels`      | `LOG_LEVEL=<level>[,<source:level>,...]`  |
| Destination | `setLogSettingsDestination` | `LOG_DESTINATION=stdout\|stderr\|@<path>` |
| Color       | `setLogSettingsColor `      | `LOG_COLOR=auto\|always\|never`           |
| Breakpoint  | `setLogSettingsBreakpoint`  | `LOG_BREAKPOINT=<number>`                 |
| Concurrency | `setLogSettingsConcurrency` | `LOG_CONCURRENCY=<number>`                |

## Advanced Usage

Add our environment variable parser to your own,

```hs
data AppSettings = AppSettings
  { appDryRun :: Bool
  , appLogSettings :: LogSettings
  , -- ...
  }

loadAppSettings :: IO AppSettings
loadAppSettings = Env.parse id $ AppSettings
  <$> var switch "DRY_RUN" mempty
  <*> LogSettingsEnv.parser
  <*> -- ...
```

Load a `Logger` into your `App` type and define `HasLogger`,

```hs
data App = App
  { appSettings :: AppSettings
  , appLogger :: Logger
  , -- ...
  }

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x { appLogger = y }

loadApp :: IO App
loadApp = do
  appSettings <- loadAppSettings
  appLogger <- newLogger $ appLogSettings appSettings
  -- ...
  pure App {..}
```

Use `runLoggerLoggingT`,

```hs
runAppT :: App -> ReaderT App (LoggingT IO) a -> IO a
runAppT app f = runLoggerLoggingT app $ runReaderT f app
```

## Use without `LoggingT`

If your app monad is not a transformer stack containing `LoggingT` (ex: the
[ReaderT pattern](https://www.fpcomplete.com/blog/readert-design-pattern/)), you
can derive `MonadLogger` via `WithLogger`:

```haskell
data AppEnv = AppEnv
  { appLogger :: Logger
  -- ...
  }

instance HasLogger AppEnv where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}

newtype App a = App
  { unApp :: ReaderT AppEnv IO a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppEnv
    )
  deriving (MonadLogger, MonadLoggerIO)
    via (WithLogger AppEnv IO)

runApp :: AppEnv -> App a -> IO a
runApp env action =
  runReaderT (unApp action) env
```

In your app you can use code written against the `MonadLogger` interface, like
the actions defined earlier:

```haskell
app :: App ()
app = do
  action1
  action2
```

In initialize the app, with `withLogger`.

```haskell
main2 :: IO ()
main2 =
  withLogger defaultLogSettings $ \logger -> do
    let appEnv =
          AppEnv
            { appLogger = logger
            -- ...
            }
    runApp appEnv app
```

## Integration with RIO

```hs
data App = App
  { appLogFunc :: LogFunc
  , -- ...
  }

instance HasLogFuncApp where
  logFuncL = lens appLogFunc $ \x y -> x { logFunc = y }

runApp :: MonadIO m => RIO App a -> m a
runApp f = runSimpleLoggingT $ do
  loggerIO <- askLoggerIO

  let
    logFunc = mkLogFunc $ \cs source level msg -> loggerIO
      (callStackLoc cs)
      source
      (fromRIOLevel level)
      (getUtf8Builder msg)

  app <- App logFunc
    <$> -- ...
    <*> -- ...

  runRIO app $ f

callStackLoc :: CallStack -> Loc
callStackLoc = undefined

fromRIOLevel :: RIO.LogLevel -> LogLevel
fromRIOLevel = undefined
```

## Integration with Amazonka

```hs
data App = App
  { appLogger :: Logger
  , appAWS :: AWS.Env
  }

instance HasLogger App where
  -- ...

runApp :: MonadUnliftIO m => ReaderT App m a -> m a
runApp f =
  withLogger defaultLogSettings $ \logger -> do
    aws <- runWithLogger logger awsDiscover
    runReaderT f $ App logger aws

awsDiscover :: (MonadIO m, MonadLoggerIO m) => m AWS.Env
awsDiscover = do
  loggerIO <- askLoggerIO
  env <- liftIO $ AWS.newEnv AWS.discover
  pure $ env
    { AWS.envLogger = \level msg -> do
      loggerIO
        defaultLoc -- TODO: there may be a way to get a CallStack/Loc
        "Amazonka"
        (case level of
          AWS.Info -> LevelInfo
          AWS.Error -> LevelError
          AWS.Debug -> LevelDebug
          AWS.Trace -> LevelOther "trace"
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
waiMiddleware app =
  addThreadContext ["app" .= ("my-app" :: Text)]
    $ requestLogger app
    $ defaultMiddlewaresNoLogging
```

## Integration with Warp

```hs
instance HasLogger App where
  -- ...

warpSettings :: App -> Settings
warpSettings app = setOnException onEx $ defaultSettings
 where
  onEx _req ex =
    when (Network.Wai.Handler.Warp.defaultShouldDisplayException ex)
      $ runWithLogger app
      $ logError
      $ "Warp exception" :# ["exception" .= displayException ex]
```

## Integration with Yesod

```hs
instance HasLogger App where
  -- ...

instance Yesod App where
  -- ...

  messageLoggerSource app _logger loc source level msg =
    runWithLogger app $ monadLoggerLog loc source level msg
```

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)

## [_Unreleased_](https://github.com/freckle/blammo/compare/Blammo-v2.1.3.0...main)

## [v2.1.3.0](https://github.com/freckle/blammo/compare/v2.1.2.0...Blammo-v2.1.3.0)

- Add `setLogSettingsColors` to support customizing colors

## [v2.1.2.0](https://github.com/freckle/blammo/compare/v2.1.1.0...Blammo-v2.1.2.0)

- Add `setLoggerReformat`

## [v2.1.1.0](https://github.com/freckle/blammo/compare/v2.1.0.0...Blammo-v2.1.1.0)

- Accept special value `null` for `LOG_DESTINATION` as a synonym for the null
  device (`/dev/null` or `\\.\NUL` on windows).

## [v2.1.0.0](https://github.com/freckle/blammo/compare/v2.0.0.0...Blammo-v2.1.0.0)

Removes less frequently used definitions from the main `Blammo.Logging` module
into other modules.

- Moved from `Blammo.Logging` to new module `Blammo.Logging.ThreadContext`:
  `MonadMask`, `withThreadContext`, `myThreadContext`, `Pair`.
- Removed from `Blammo.Logging` (still available in `Blammo.Logging.LogSettings`):
  `LogSettings`, `LogDestination (..)`, `LogFormat (..)`, `defaultLogSettings`,
  `LogColor (..)`, `setLogSettingsLevels`, `setLogSettingsDestination`,
  `setLogSettingsFormat`, `setLogSettingsColor`, `setLogSettingsBreakpoint`,
  `setLogSettingsConcurrency`.
- Moved from `Blammo.Logging` to new module `Blammo.Logging.Setup`:
  `HasLogger (..)`, `withLogger`, `newLogger`, `runLoggerLoggingT`, `LoggingT`,
  `WithLogger (..)`, `runWithLogger`

`Blammo.Logging.Simple` has been expanded to include reëxports of:

- `Blammo.Logging.LogSettings`
- `Blammo.Logging.Setup`
- `Blammo.Logging.ThreadContext`

## [v2.0.0.0](https://github.com/freckle/blammo/compare/v1.2.1.0...Blammo-v2.0.0.0)

- Remove module `Network.Wai.Middleware.Logging`. It is moved to a new
  package, `Blammo-wai`.

## [v1.2.1.0](https://github.com/freckle/blammo/compare/1.2.0.0...v1.2.1.0)

- Add `Blammo.Logging.Simple.withLoggerEnv`

## [v1.2.0.0](https://github.com/freckle/blammo/compare/v1.1.3.0...v1.2.0.0)

- New in `Blammo.Logging`: `withLogger`, `WithLogger(..), runWithLogger`
- New in `Blammo.Logging.Logger`: `runLogAction`
- WAI middleware no longer performs a log flush. Wrap your entire application
  in either `withLoggerLoggingT` or `withLogger` to ensure a log flush at
  application shutdown.

## [v1.1.3.0](https://github.com/freckle/blammo/compare/v1.1.2.3...v1.1.3.0)

- Update fast-logger to fix log flushing bug, and remove 0.1s delay that was
  introduced as a workaround.

## [v1.1.2.3](https://github.com/freckle/blammo/compare/v1.1.2.2...v1.1.2.3)

- Add small delay (0.1s) in `flushLogger` to work around fast-logger bug

## [v1.1.2.2](https://github.com/freckle/blammo/compare/v1.1.2.1...v1.1.2.2)

- Don't automatically colorize if `TERM=dumb` is found in ENV
- Respect [`NO_COLOR`](http://no-color.org/)
- Automatically adjust log concurrency based on `LOG_FORMAT`:

  Disable concurrency for `tty` (making that the new default) and enable it for
  `json`. Setting `LOG_CONCURRENCY` will still be respected.

## [v1.1.2.1](https://github.com/freckle/blammo/compare/v1.1.2.0...v1.1.2.1)

- Add various `getColors*` helper functions

## [v1.1.2.0](https://github.com/freckle/blammo/compare/v1.1.1.2...v1.1.2.0)

- Add `Blammo.Logging.LogSettings.LogLevels`

## [v1.1.1.2](https://github.com/freckle/blammo/compare/v1.1.1.1...v1.1.1.2)

- Fix bug in `LOG_CONCURRENCY` parser

## [v1.1.1.1](https://github.com/freckle/blammo/compare/v1.1.1.0...v1.1.1.1)

- Add `getLogSettingsConcurrency`
- Add `getLoggerShouldColor`
- Add `pushLoggerStr` & `pushLoggerStrLn`
- Add `getLoggerLogSettings`

## [v1.1.1.0](https://github.com/freckle/blammo/compare/v1.1.0.0...v1.1.1.0)

- Terminal formatter: align attributes vertically if the message goes over a
  certain number of characters (default 120).
- Adds `{get,set}LogSettingsBreakpoint` and `LOG_BREAKPOINT` parsing

## [v1.1.0.0](https://github.com/freckle/blammo/compare/v1.0.3.0...v1.1.0.0)

- Add `flushLogger`
- Ensure log is flushed even on exceptions.

## [v1.0.3.0](https://github.com/freckle/blammo/compare/v1.0.2.3...v1.0.3.0)

- Add `Env.{parse,parser}With` functions for parsing 'LogSettings' from
  environment variables with custom defaults.

## [v1.0.2.3](https://github.com/freckle/blammo/compare/v1.0.2.2...v1.0.2.3)

- Fix for localhost `clientIp` value in `requestLogger` ([#18](https://github.com/freckle/blammo/issues/18))

## [v1.0.2.2](https://github.com/freckle/blammo/compare/v1.0.2.1...v1.0.2.2)

- Support down to LTS 12.26 / GHC 8.4

## [v1.0.2.1](https://github.com/freckle/blammo/compare/v1.0.1.1...v1.0.2.1)

- Add configurability to `requestLogger`, set `LogSource` by default
- Add ability to capture and retrieve logged messages, for testing

## [v1.0.1.1](https://github.com/freckle/blammo/compare/v1.0.0.1...v1.0.1.1)

- Add `addThreadContextFromRequest`, a wai `Middleware` for adding context using
  information from the `Request`.

## [v1.0.0.1](https://github.com/freckle/blammo/compare/v1.0.0.0...v1.0.0.1)

- Relax lower bounds, support GHC 8.8

## [v1.0.0.0](https://github.com/freckle/blammo/tree/v1.0.0.0)

First tagged release.

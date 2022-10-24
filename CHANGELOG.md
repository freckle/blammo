## [_Unreleased_](https://github.com/freckle/blammo/compare/v1.1.0.0...main)

## [v1.1.0.0](https://github.com/freckle/blammo/compare/v1.0.3.0...v1.1.0.0)

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

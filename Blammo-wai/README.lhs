# Blammo-wai

Integration of [Blammo](https://hackage.haskell.org/package/Blammo)
with [wai](https://hackage.haskell.org/package/wai).

<!--
```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Main (module Main) where

import Prelude

import Control.Exception (displayException)
import Control.Monad (when)
import Data.Aeson
import Data.Text (Text)
import Control.Lens (lens)
```
-->

```haskell
-- Blammo
import Blammo.Logging.Simple

-- wai
import Network.Wai (Middleware)

-- Blammo-wai
import Network.Wai.Middleware.Logging

-- warp
import qualified Network.Wai.Handler.Warp as Warp

-- yesod
import Yesod.Core (Yesod)
import qualified Yesod.Core as Yesod
```

<!--
```haskell
main :: IO ()
main = pure ()
```
-->

```haskell
data App = App
  { appLogger :: Logger
  -- etc.
  }

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}
```

## Integration with WAI

```haskell
waiMiddleware :: App -> Middleware
waiMiddleware app =
  addThreadContext ["app" .= ("my-app" :: Text)]
    . requestLogger app
    . Yesod.defaultMiddlewaresNoLogging
```

## Integration with Warp

```haskell
warpSettings :: App -> Warp.Settings
warpSettings app = Warp.setOnException onEx $ Warp.defaultSettings
 where
  onEx _req ex =
    when (Warp.defaultShouldDisplayException ex)
      $ runWithLogger app
      $ logError
      $ "Warp exception" :# ["exception" .= displayException ex]
```

## Integration with Yesod

<!--
```haskell
instance Yesod.RenderRoute App where
  data Route App deriving stock Eq
  renderRoute = \case{}
```
-->

```haskell
instance Yesod App where
  messageLoggerSource app _logger loc source level msg =
    runWithLogger app $ monadLoggerLog loc source level msg
```

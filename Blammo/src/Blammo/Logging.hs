module Blammo.Logging
  ( LogLevel (..)
  , Logger

    -- * Re-exports from "Control.Monad.Logger.Aeson"

    -- ** Messages
  , Message (..)
  , (.=)
  , Series

    -- ** Classes
  , MonadLogger (..)
  , MonadLoggerIO (..)

    -- ** Common logging functions

    -- | Import "Control.Monad.Logger.Aeson" if you want more

    -- *** Implicit call stack, no 'LogSource'
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logOther

    -- *** Implicit call stack, with 'LogSource'
  , LogSource
  , logDebugNS
  , logInfoNS
  , logWarnNS
  , logErrorNS
  , logOtherNS
  ) where

import Blammo.Logging.Logger
import Control.Monad.Logger.Aeson
import Data.Aeson (Series)

module Blammo.Logging
  ( LogSettings
  , LogLevel (..)
  , LogDestination (..)
  , LogFormat (..)
  , LogColor (..)
  , defaultLogSettings
  , setLogSettingsLevels
  , setLogSettingsDestination
  , setLogSettingsFormat
  , setLogSettingsColor
  , setLogSettingsBreakpoint
  , setLogSettingsConcurrency
  , Logger
  , HasLogger (..)
  , newLogger
  , runLoggerLoggingT

    -- * Re-exports from "Control.Monad.Logger.Aeson"

    -- ** Messages
  , Message (..)
  , (.=)
  , Series

    -- ** Thread Context
  , MonadMask
  , withThreadContext
  , myThreadContext
  , Pair

    -- ** Transformer
  , MonadLogger (..)
  , MonadLoggerIO (..)
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
  , LogSource
  , logDebugNS
  , logInfoNS
  , logWarnNS
  , logErrorNS
  , logOtherNS
  ) where

import Blammo.Logging.LogSettings
import Blammo.Logging.Logger
import Blammo.Logging.LoggingT
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger.Aeson hiding (LoggingT (..), filterLogger)
import Data.Aeson (Series)
import Data.Aeson.Types (Pair)

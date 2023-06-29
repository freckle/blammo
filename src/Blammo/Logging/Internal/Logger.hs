module Blammo.Logging.Internal.Logger
  ( Logger (..)
  , HasLogger (..)
  ) where

import Prelude

import Blammo.Logging.LogSettings
import Blammo.Logging.Test hiding (getLoggedMessages)
import Control.Lens (Lens')
import Control.Monad.Logger.Aeson
import Data.ByteString (ByteString)
import System.Log.FastLogger (LoggerSet)

data Logger = Logger
  { lLogSettings :: LogSettings
  , lLoggerSet :: LoggerSet
  , lReformat :: LogLevel -> ByteString -> ByteString
  , lShouldLog :: LogSource -> LogLevel -> Bool
  , lShouldColor :: Bool
  , lLoggedMessages :: Maybe LoggedMessages
  }

class HasLogger env where
  loggerL :: Lens' env Logger

instance HasLogger Logger where
  loggerL = id

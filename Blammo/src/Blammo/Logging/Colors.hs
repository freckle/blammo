-- | Generic facilities for adding terminal escapes to 'Text'
--
-- Recommended usage:
--
-- @
-- Colors {..} <- 'getColorsLogger' -- for example
-- pure $ "This text will be " <> red "red" <> "."
-- @
module Blammo.Logging.Colors
  ( Colors (..)
  , noColors
  , getColors
  , getColorsLogger
  , getColorsHandle
  , getColorsStdout
  , getColorsStderr
  ) where

import Prelude

import Blammo.Logging.Internal.Colors
import Blammo.Logging.Internal.Logger
import Blammo.Logging.LogSettings (shouldColorHandle)
import Control.Lens (to, view)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import System.IO (Handle, stderr, stdout)

-- | Return 'Colors' consistent with whatever your logging is doing
getColorsLogger :: (MonadReader env m, HasLogger env) => m Colors
getColorsLogger = view $ loggerL . to (getColors . lShouldColor)

-- | Return 'Colors' consistent with logging, but for 'Handle'
--
-- This is useful if you are building text to print to a handle that is not the
-- one you are logging to.
--
-- For example, say you are using,
--
-- @
-- LOG_COLOR=auto
-- LOG_DESTINATION=@some-file.log
-- @
--
-- That will not log with color, so 'getColorsLogger' will be 'noColor'. If
-- you're building other text to be printed out, you probably want to respect
-- that @LOG_COLOR=auto@, so you would use this function instead.
getColorsHandle
  :: (MonadIO m, MonadReader env m, HasLogger env) => Handle -> m Colors
getColorsHandle h = do
  ls <- view $ loggerL . to lLogSettings
  getColors <$> shouldColorHandle ls h

-- | Short-cut for @'getColorsHandle' 'stdout'@
getColorsStdout :: (MonadIO m, MonadReader env m, HasLogger env) => m Colors
getColorsStdout = getColorsHandle stdout

-- | Short-cut for @'getColorsHandle' 'stderr'@
getColorsStderr :: (MonadIO m, MonadReader env m, HasLogger env) => m Colors
getColorsStderr = getColorsHandle stderr

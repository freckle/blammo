-- | Produce a 'LogSettings' by reading environment variables
--
-- - @LOG_LEVEL@: a known log level (case insensitive) and optional levels by
--   source. See "Logging.LogSettings.LogLevels".
--
-- - @LOG_DESTINATION@: the string @stderr@, @stdout@ or @null@, (case
--   sensitive), or @\@{path}@ to log to the file at @path@. Unrecognized values
--   will produce an error.
--
-- - @LOG_FORMAT@: the string @tty@ or @json@. Unrecognized values will produce
--   an error.
--
-- - @LOG_COLOR@: the string @auto@, @always@, or @never@. Other values may be
--   recognized (e.g. @yes@ or @no@) but should not be relied on. Unrecognized
--   values will produce an error
--
-- - @LOG_BREAKPOINT@: a number representing the column-width at which to break
--   into multi-line format.
--
-- - @LOG_CONCURRENCY@: number of log buffers to use. More will perform faster
--   but result in out-of-order delivery. This is automatically disabled for
--   @LOG_FORMAT=tty@ and set to /number-of-cores/ for @LOG_FORMAT=json@.
--
-- - @NO_COLOR@: if present and non-empty, behave as if @LOG_COLOR=never@
--
-- - @TERM@: if present and the value @dumb@, behave as if @LOG_COLOR=never@.
--
-- This module is meant to be imported @qualified@.
--
-- @
-- import Blammo.Logging
-- import qualified Logging.LogSettings.Env as Env
--
-- main :: IO ()
-- main = do
--   logger <- 'newLogger' =<< Env.'parse'
--   'runLoggerLoggingT' logger $ -- ...
-- @
module Blammo.Logging.LogSettings.Env
  ( parse
  , parser
    -- | Specifying defaults other than 'defaultLogSettings'
    --
    -- For example, if you want logging to go to @stderr@ by default, but still
    -- support @LOG_DESTINATION@,
    --
    -- @
    -- settings <- Env.'parseWith'
    --   $ 'setLogSettingsDestination' 'LogDestinationStderr' 'defaultLogSettings'
    -- @
  , parseWith
  , parserWith
  ) where

import Prelude

import Blammo.Logging.LogSettings
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Semigroup (Endo (..))
import Data.Text (Text)
import Env hiding (parse)
import qualified Env
import Text.Read (readEither)

parse :: IO LogSettings
parse = parseWith defaultLogSettings

parser :: Parser Error LogSettings
parser = parserWith defaultLogSettings

parseWith :: LogSettings -> IO LogSettings
parseWith = Env.parse id . parserWith

parserWith :: LogSettings -> Parser Error LogSettings
parserWith defaults =
  flip (appEndo . mconcat) defaults
    <$> sequenceA
      [ endoVar readLogLevels setLogSettingsLevels "LOG_LEVEL"
      , endoVar readLogDestination setLogSettingsDestination "LOG_DESTINATION"
      , endoVar readLogColor setLogSettingsColor "LOG_COLOR"
      , endoVar readEither setLogSettingsBreakpoint "LOG_BREAKPOINT"
      , endoVar readEither (setLogSettingsConcurrency . Just) "LOG_CONCURRENCY"
      , endoVar readLogFormat setLogSettingsFormat "LOG_FORMAT"
      , endoSwitch (setLogSettingsColor LogColorNever) "NO_COLOR"
      , endoOn "dumb" (setLogSettingsColor LogColorNever) "TERM"
      ]

endoVar
  :: (AsUnset e, AsUnread e)
  => (String -> Either String a)
  -- ^ How to parse the value
  -> (a -> b -> b)
  -- ^ How to turn the parsed value into a setter
  -> String
  -> Parser e (Endo b)
endoVar reader setter x = var (endo reader setter) x $ def mempty

endo
  :: AsUnread e
  => (String -> Either String a)
  -- ^ How to parse the value
  -> (a -> b -> b)
  -- ^ How to turn the parsed value into a setter
  -> Reader e (Endo b)
endo reader setter x = first unread $ Endo . setter <$> reader x

endoSwitch :: (a -> a) -> String -> Parser e (Endo a)
endoSwitch f x = endoWhen f <$> switch x mempty

endoOn :: AsUnset e => Text -> (a -> a) -> String -> Parser e (Endo a)
endoOn val f x = endoWhen f . (== Just val) <$> optional (var str x mempty)

endoWhen
  :: (a -> a)
  -> Bool
  -> Endo a
endoWhen f = bool mempty (Endo f)

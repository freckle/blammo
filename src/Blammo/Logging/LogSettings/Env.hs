-- | Produce a 'LogSettings' by reading environment variables
--
-- - @LOG_LEVEL@: a known log level (case insensitive) and optional levels by
--   source. See "Logging.LogSettings.LogLevels".
--
-- - @LOG_DESTINATION@: the string @stderr@ or @stdout@ (case sensitive), or
--   @\@{path}@ to log to the file at @path@. Unrecognized values will produce
--   and error.
--
-- - @LOG_FORMAT@: the string @tty@ or @json@. Unrecognized values will produce
--   an error.
--
-- - @LOG_COLOR@: the string @auto@, @always@, or @never@. Other values may be
--   recognized (e.g. @yes@ or @no@) but should not be relied on. Unrecognized
--   values will produce an error
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
--
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
  --
  , parseWith
  , parserWith
  ) where

import Prelude

import Blammo.Logging.LogSettings
import Data.Bifunctor (first)
import Data.Semigroup (Endo(..))
import Env hiding (parse)
import qualified Env

parse :: IO LogSettings
parse = parseWith defaultLogSettings

parser :: Parser Error LogSettings
parser = parserWith defaultLogSettings

parseWith :: LogSettings -> IO LogSettings
parseWith = Env.parse id . parserWith

-- brittany-next-binding --columns 100

parserWith :: LogSettings -> Parser Error LogSettings
parserWith defaults = ($ defaults) . appEndo . mconcat <$> sequenceA
  [ var (endo readLogLevels setLogSettingsLevels) "LOG_LEVEL" (def mempty)
  , var (endo readLogDestination setLogSettingsDestination) "LOG_DESTINATION" (def mempty)
  , var (endo readLogFormat setLogSettingsFormat) "LOG_FORMAT" (def mempty)
  , var (endo readLogColor setLogSettingsColor) "LOG_COLOR" (def mempty)
  ]

endo
  :: AsUnread e
  => (String -> Either String a)
  -- ^ How to parse the value
  -> (a -> b -> b)
  -- ^ How to turn the parsed value into a setter
  -> Reader e (Endo b)
endo reader setter x = first unread $ Endo . setter <$> reader x

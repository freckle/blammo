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
-- import Logging
-- import qualified Logging.LogSettings.Env as Env
--
-- main :: IO ()
-- main = do
--   logger <- 'newLogger' =<< Env.'parse'
--   'runLoggerLoggingT' logger $ -- ...
-- @
--
module Logging.LogSettings.Env
  ( parse
  , parser
  ) where

import Prelude

import Data.Bifunctor (first)
import Data.Semigroup (Endo(..))
import Env hiding (parse)
import qualified Env
import Logging.LogSettings

parse :: IO LogSettings
parse = Env.parse id parser

-- brittany-next-binding --columns 100

parser :: Parser Error LogSettings
parser = ($ defaultLogSettings) . appEndo . mconcat <$> sequenceA
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

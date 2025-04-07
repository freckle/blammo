-- | Backwards-compatible extension to a simple 'LogLevel' parser/filter
--
-- Assume you are using this library/module to parse a @LOG_LEVEL@ environment
-- variable, which is used to filter your logs.
--
-- Running,
--
-- @
-- LOG_LEVEL=warn ./my-program
-- @
--
-- Will do what you expect: filter all logging to only those messages
-- at-or-above @warn@ level.
--
-- While,
--
-- @
-- LOG_LEVEL=debug ./my-program
-- @
--
-- Will enable debug logging throughout.
--
-- This is all un-surprising and this module does not change behavior in this
-- case whatsoever. But let's say that is entirely too noisy. Because you're
-- using Amazonka and persistent, and have correctly integrated your main
-- logging with them, you are now getting /tons/ of spam from their very-chatty
-- debug logs, and its drowning out the application debug logs you were hoping
-- to see.
--
-- Well, now can do this:
--
-- @
-- LOG_LEVEL="debug,Amazonka:info,SQL:warn" ./my-program
-- @
--
-- And suddenly your application's debug logs are standing out again, because
-- everything from the Amazonka source is filtered to info and the SQL source is
-- filtered to warn.
--
-- The format parsed by 'readLogLevels' is:
--
-- @
-- [<source:level>, ...,]<level>[, <source:level>, ...]
-- @
--
-- Where @<level>@ defines the minimum level for anything not overridden by
-- source. If you go on to add any @<source:level>@ pairs, that will change the
-- minimum level for messages from that source.
module Blammo.Logging.LogSettings.LogLevels
  ( LogLevels
  , LogLevel (..)
  , newLogLevels
  , readLogLevels
  , showLogLevels
  , shouldLogLevel
  , defaultLogLevels
  ) where

import Prelude

import Control.Monad.Logger.Aeson
import Data.Either (partitionEithers)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

data LogLevels = LogLevels
  { llDefaultLevel :: LogLevel
  , llSourceLevels :: Map LogSource LogLevel
  }
  deriving stock (Eq, Show)

newLogLevels :: LogLevel -> [(LogSource, LogLevel)] -> LogLevels
newLogLevels level sourceLevels =
  LogLevels
    { llDefaultLevel = level
    , llSourceLevels = Map.fromList sourceLevels
    }

readLogLevels :: String -> Either String LogLevels
readLogLevels s = toLogLevels . partitionEithers =<< traverse readPiece pieces
 where
  toLogLevels = \case
    ([], _) -> invalid "no level present"
    (_ : _ : _, _) -> invalid "more than one level present"
    ([level], sourceLevels) -> pure $ newLogLevels level sourceLevels

  readPiece t = case T.breakOn ":" t of
    (a, ":") -> invalid $ "no level for source " <> unpack a
    (a, b) | T.null a -> invalid $ "no source for level" <> unpack b
    (a, b) | T.null b -> pure $ Left $ readLogLevel a
    (a, b) -> pure $ Right (a, readLogLevel $ T.drop 1 b)

  pieces = filter (not . T.null) $ map T.strip $ T.splitOn "," $ pack s

  invalid reason = Left $ "Invalid log level " <> s <> ", " <> reason

readLogLevel :: Text -> LogLevel
readLogLevel t = case T.toLower t of
  "debug" -> LevelDebug
  "info" -> LevelInfo
  "warn" -> LevelWarn
  "error" -> LevelError
  _ -> LevelOther t

showLogLevels :: LogLevels -> String
showLogLevels LogLevels {..} =
  unpack
    $ T.intercalate ","
    $ showLogLevel llDefaultLevel
      : map
        (\(s, l) -> s <> ":" <> showLogLevel l)
        (Map.toList llSourceLevels)

showLogLevel :: LogLevel -> Text
showLogLevel = \case
  LevelDebug -> "debug"
  LevelInfo -> "info"
  LevelWarn -> "warn"
  LevelError -> "error"
  LevelOther t -> t

shouldLogLevel :: LogLevels -> LogSource -> LogLevel -> Bool
shouldLogLevel LogLevels {..} source = (`lgte` minLevel)
 where
  minLevel = fromMaybe llDefaultLevel $ Map.lookup source llSourceLevels

defaultLogLevels :: LogLevels
defaultLogLevels =
  LogLevels {llDefaultLevel = LevelInfo, llSourceLevels = Map.empty}

-- | Like '(>=)', but treats @'LevelOther' "trace"@ as below 'LevelDebug'
--
-- Normally, 'LevelOther' is the highest level, but it's common to use the
-- @trace@ level as more verbose than @debug@. With this comparison in use, we
-- can safely use @'LevelOther' "trace"@ for that.
lgte :: LogLevel -> LogLevel -> Bool
lgte _ (LevelOther x) | T.toLower x == "trace" = True
lgte (LevelOther x) _ | T.toLower x == "trace" = False
lgte a b = a >= b

{-# LANGUAGE TupleSections #-}

-- | Colorful logging for humans
--
-- Lines are formatted as
--
-- @
-- {timestamp} [{level}] {message} {details}
-- @
--
-- @level@ is padded to 9 characters and @message@ is padded to 31. This means
-- things will align as long as values are shorter than that. Longer values will
-- overflow (not be truncated).
--
-- This format was designed to match Python's
-- [structlog](https://www.structlog.org/en/stable/) package in its default
-- configuration.
module Blammo.Logging.Terminal
  ( reformatTerminal
  ) where

import Prelude

import Blammo.Logging.Terminal.Doc
import Control.Monad.Logger.Aeson
import Data.Aeson
import qualified Data.Aeson.Compat as Key
import qualified Data.Aeson.Compat as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import Prettyprinter hiding (list)
import Prettyprinter.Util (reflow)

reformatTerminal :: LogLevel -> LoggedMessage -> Doc Ann
reformatTerminal logLevel LoggedMessage {..} =
  prettyTimestamp loggedMessageTimestamp
    <+> prettyLogLevel logLevel
    <+> align
      ( fill 31 (prettyMessage loggedMessageText)
          <> group (flatAlt multiline oneline)
      )
 where
  oneline = " " <> hsep metas
  multiline = hardline <> vsep metas

  metas :: [Doc Ann]
  metas =
    map (uncurry prettyPair)
      $ maybe mempty (pure . ("source",) . String) loggedMessageLogSource
        <> KeyMap.toList loggedMessageThreadContext
        <> KeyMap.toList loggedMessageMeta

prettyTimestamp :: UTCTime -> Doc Ann
prettyTimestamp =
  annotate AnnTimestamp . pretty . formatTime defaultTimeLocale "%F %X"

prettyLogLevel :: LogLevel -> Doc Ann
prettyLogLevel l = enclose "[" "]" $ annotate (AnnByLevel l) $ levelDoc 9
 where
  levelDoc :: Int -> Doc ann
  levelDoc n =
    fill n $ case l of
      LevelDebug -> "debug"
      LevelInfo -> "info"
      LevelWarn -> "warn"
      LevelError -> "error"
      LevelOther x -> pretty $ T.take n x

prettyMessage :: Text -> Doc Ann
prettyMessage = vsep . map reflow . T.lines

prettyPair :: Key -> Value -> Doc Ann
prettyPair k v =
  annotate AnnKey (pretty $ Key.toText k)
    <> "="
    <> annotate AnnValue (fromValue v)

fromValue :: Value -> Doc Ann
fromValue = \case
  Object m -> list "{" "}" $ map (uncurry pair) $ KeyMap.toList m
  Array a -> list "[" "]" $ map fromValue $ V.toList a
  String x -> annotate AnnString $ pretty x
  Number n -> annotate AnnNumber $ pretty $ dropSuffix ".0" $ pack $ show n
  Bool b -> annotate AnnBoolean $ pretty $ show b
  Null -> annotate AnnNull "null"

pair :: Key -> Value -> Doc Ann
pair k v =
  pretty (Key.toText k)
    <> annotate AnnPunctuation ": "
    <> fromValue v

list :: Doc Ann -> Doc Ann -> [Doc Ann] -> Doc Ann
list l r =
  enclose (annotate AnnPunctuation l) (annotate AnnPunctuation r)
    . hcat
    . punctuate (annotate AnnPunctuation ", ")

dropSuffix :: Text -> Text -> Text
dropSuffix suffix t = fromMaybe t $ T.stripSuffix suffix t

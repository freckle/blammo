module Logging.Settings.Env
  ( parse
  , parser
  ) where

import Prelude

import Data.Bifunctor (first)
import Data.Semigroup (Endo(..))
import Env hiding (parse)
import qualified Env
import Logging.Settings

parse :: IO LogSettings
parse = Env.parse id parser

-- brittany-next-binding --columns 100

parser :: Parser Error LogSettings
parser = ($ defaultLogSettings) . appEndo . mconcat <$> sequenceA
  [ var (endo readLogLevel setLogSettingsLevel) "LOG_LEVEL" (def mempty)
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

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import Blammo.Logging.Simple
import Data.Text (Text)

main :: IO ()
main = runSimpleLoggingT $ do
  logInfo $ "Rolling down stairs" :# []
  logInfo $ "Alone" :# ["inPairs" .= True]
  logWarn $ "Rolling" :# ["where" .= [t "over", t "your", t "neighbor's dog!"]]
  logInfo $ "What's great for a snack" :# []
  logInfo $ "and fits on your back?" :# []
  logInfo $ "It's Log, Log, Log!" :# []
  logDebug $ "It's Lo-og, Lo-og," :# ["it's" .= [t "big", "heavy", "wood"]]
  logInfo $ "It's Lo-og, Lo-og," :# []
  logInfo $ "It's better than bad, it's good!" :# []

t :: Text -> Text
t = id

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude

import Blammo.Logging.Simple
import Data.Text (Text)

main :: IO ()
main = runSimpleLoggingT $ do
  logInfo
    $ "Here is a normal line"
      :# ["vehicle" .= t "firetruck", "accountNumber" .= t "are"]
  logInfo
    $ "And here are really wide lines"
      :# [ "vehicle" .= t "firetruck"
         , "accountNumber" .= t "are"
         , "url" .= t "http://example.com/foo/bar/baz/bat"
         , "frog" .= t "hippity hoppity"
         , "there" .= t "do"
         , "state" .= t "PA"
         , "hey" .= t "hio"
         ]
  logInfo
    $ "Some more log lines"
      :# [ "orange" .= t "how"
         , "there" .= t "do"
         , "state" .= t "PA"
         , "url" .= t "http://example.com/foo/bar/baz/bat"
         , "accountNumber" .= t "are"
         , "frog" .= t "hippity hoppity"
         , "hey" .= t "hio"
         ]
  logInfo
    $ "Such a big attribute list"
      :# [ "apple" .= t "hi"
         , "orange" .= t "how"
         , "accountNumber" .= t "are"
         , "state" .= t "PA"
         , "url" .= t "http://example.com/foo/bar/baz/bat"
         , "frog" .= t "hippity hoppity"
         , "hey" .= t "hio"
         , "there" .= t "do"
         ]

t :: Text -> Text
t = id

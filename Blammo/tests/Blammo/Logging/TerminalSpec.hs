{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Blammo.Logging.TerminalSpec
  ( spec
  ) where

import Prelude

import Blammo.Logging
import Blammo.Logging.Logger (LoggedMessage (..))
import Blammo.Logging.Terminal
import Blammo.Logging.Terminal.Doc
import Data.Aeson (object)
import Data.Aeson.Types (Object, Pair, Value (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Test.Hspec

spec :: Spec
spec = do
  describe "reformatTerminal" $ do
    it "reformats LoggedMessages with complex attributes" $ do
      let
        lm =
          LoggedMessage
            { loggedMessageTimestamp =
                UTCTime
                  { utctDay = fromGregorian 2022 1 1
                  , utctDayTime = 0
                  }
            , loggedMessageLevel = LevelInfo
            , loggedMessageLoc = Nothing
            , loggedMessageLogSource = Just "app"
            , loggedMessageThreadContext = keyMap ["x" .= object ["y" .= True]]
            , loggedMessageText = "I'm a log message"
            , loggedMessageMeta = keyMap ["a" .= [1 :: Int, 2, 3]]
            }

        expected =
          mconcat
            [ "2022-01-01 00:00:00 [info     ] I'm a log message              "
            , " source=app x={y: True} a=[1, 2, 3]"
            ]

      renderTerminal False 120 LevelInfo lm `shouldBe` expected

    it "moves attributes to multi-line at the given breakpoint" $ do
      let
        lm =
          LoggedMessage
            { loggedMessageTimestamp =
                UTCTime
                  { utctDay = fromGregorian 2022 1 1
                  , utctDayTime = 0
                  }
            , loggedMessageLevel = LevelInfo
            , loggedMessageLoc = Nothing
            , loggedMessageLogSource = Just "app"
            , loggedMessageThreadContext = mempty
            , loggedMessageText = "I'm a log message"
            , loggedMessageMeta =
                keyMap
                  [ "a" .= ("aaaaaaaaa" :: Text)
                  , "b" .= ("aaaaaaaaa" :: Text)
                  , "c" .= ("aaaaaaaaa" :: Text)
                  , "d" .= ("aaaaaaaaa" :: Text)
                  ]
            }

        single =
          mconcat
            [ "2022-01-01 00:00:00 [info     ] I'm a log message              "
            , " source=app a=aaaaaaaaa b=aaaaaaaaa c=aaaaaaaaa d=aaaaaaaaa"
            ]

        multi =
          mconcat
            [ "2022-01-01 00:00:00 [info     ] I'm a log message              \n"
            , "                                source=app\n"
            , "                                a=aaaaaaaaa\n"
            , "                                b=aaaaaaaaa\n"
            , "                                c=aaaaaaaaa\n"
            , "                                d=aaaaaaaaa"
            ]

        breakpoint = T.length single

      renderTerminal False breakpoint LevelInfo lm `shouldBe` single
      renderTerminal False (breakpoint - 1) LevelInfo lm
        `shouldBe` multi

  it "reflows long and multi-line messages" $ do
    let
      lm =
        LoggedMessage
          { loggedMessageTimestamp =
              UTCTime
                { utctDay = fromGregorian 2022 1 1
                , utctDayTime = 0
                }
          , loggedMessageLevel = LevelInfo
          , loggedMessageLoc = Nothing
          , loggedMessageLogSource = Just "app"
          , loggedMessageThreadContext = mempty
          , loggedMessageText =
              "I'm a really really really long message "
                <> "with multiple lines that are so long "
                <> "they should get reflowed at the column "
                <> "limit."
                <> "\n"
                <> "\nThey are:"
                <> "\n"
                <> "\n1- This"
                <> "\n2- That"
          , loggedMessageMeta = mempty
          }

      expected =
        mconcat
          [ "2022-01-01 00:00:00 [info     ] I'm a really really really long message with\n"
          , "                                multiple lines that are so long they should get\n"
          , "                                reflowed at the column limit.\n"
          , "\n"
          , "                                They are:\n"
          , "\n"
          , "                                1- This\n"
          , "                                2- That                         source=app"
          ]

    stripColor (renderTerminal True 80 LevelInfo lm)
      `shouldBe` expected

  it "aligns multi-line correctly even with color escapes" $ do
    let
      lm =
        LoggedMessage
          { loggedMessageTimestamp =
              UTCTime
                { utctDay = fromGregorian 2022 1 1
                , utctDayTime = 0
                }
          , loggedMessageLevel = LevelInfo
          , loggedMessageLoc = Nothing
          , loggedMessageLogSource = Just "app"
          , loggedMessageThreadContext = mempty
          , loggedMessageText = "I'm a log message"
          , loggedMessageMeta =
              keyMap
                [ "a" .= ("aaaaaaaaa" :: Text)
                , "b" .= ("aaaaaaaaa" :: Text)
                , "c" .= ("aaaaaaaaa" :: Text)
                , "d" .= ("aaaaaaaaa" :: Text)
                ]
          }

      expected =
        mconcat
          [ "2022-01-01 00:00:00 [info     ] I'm a log message              \n"
          , "                                source=app\n"
          , "                                a=aaaaaaaaa\n"
          , "                                b=aaaaaaaaa\n"
          , "                                c=aaaaaaaaa\n"
          , "                                d=aaaaaaaaa"
          ]

    stripColor (renderTerminal True 120 LevelInfo lm)
      `shouldBe` expected

keyMap :: [Pair] -> Object
keyMap ps = km where Object km = object ps

-- Removes from any '\ESC' Char to the next 'm' Char
stripColor :: Text -> Text
stripColor = snd . T.foldl' go (False, "")
 where
  go :: (Bool, Text) -> Char -> (Bool, Text)
  go (dropping, acc) = \case
    '\ESC' -> (True, acc)
    'm' | dropping -> (False, acc)
    _ | dropping -> (True, acc)
    c -> (False, T.snoc acc c)

renderTerminal :: Bool -> Int -> LogLevel -> LoggedMessage -> Text
renderTerminal c w l = renderDoc settings . reformatTerminal l
 where
  settings =
    RenderSettings
      { rsUseColor = c
      , rsPageWidth = w
      , rsAnnToAnsi = annToAnsi
      }

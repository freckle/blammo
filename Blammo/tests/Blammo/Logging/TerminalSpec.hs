{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Blammo.Logging.TerminalSpec
  ( spec
  ) where

import Prelude

import Blammo.Logging
import Blammo.Logging.Logger (LoggedMessage (..))
import Blammo.Logging.Terminal
import Data.Aeson (encode, object)
import Data.Aeson.Types (Object, Pair, Value (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import Data.Time
import Test.Hspec

spec :: Spec
spec = do
  describe "reformatTerminal" $ do
    it "passes-through none-LoggedMessages as-is" $ do
      let
        bs :: ByteString
        bs = "I'm not even JSON"

        bsJSON :: ByteString
        bsJSON = "{\"I'm\":\"Valid JSON, but not LoggedMessage\"}"

      reformatTerminal 0 False LevelInfo bs `shouldBe` bs
      reformatTerminal 0 False LevelInfo bsJSON `shouldBe` bsJSON

    it "reformats LoggedMessages with complex attributes" $ do
      let
        bs =
          BSL.toStrict $
            encode
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

      reformatTerminal 120 False LevelInfo bs `shouldBe` expected

    it "moves attributes to multi-line at the given breakpoint" $ do
      let
        bs =
          BSL.toStrict $
            encode
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

        breakpoint = BS.length single

      reformatTerminal breakpoint False LevelInfo bs `shouldBe` single
      reformatTerminal (breakpoint - 1) False LevelInfo bs `shouldBe` multi

  it "aligns multi-line correctly even with color escapes" $ do
    let
      bs =
        BSL.toStrict $
          encode
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

    stripColor (reformatTerminal 120 True LevelInfo bs) `shouldBe` expected

keyMap :: [Pair] -> Object
keyMap ps = km where Object km = object ps

-- Removes from any '\ESC' Char to the next 'm' Char
stripColor :: ByteString -> ByteString
stripColor = snd . BS8.foldl' go (False, "")
 where
  go :: (Bool, ByteString) -> Char -> (Bool, ByteString)
  go (dropping, acc) = \case
    '\ESC' -> (True, acc)
    'm' | dropping -> (False, acc)
    _ | dropping -> (True, acc)
    c -> (False, BS8.snoc acc c)

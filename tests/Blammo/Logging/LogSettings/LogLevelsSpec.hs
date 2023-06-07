module Blammo.Logging.LogSettings.LogLevelsSpec
  ( spec
  ) where

import Prelude

import Blammo.Logging.LogSettings.LogLevels
import Data.Either (isLeft, isRight)
import Test.Hspec

spec :: Spec
spec = do
  describe "readLogLevels" $ do
    it "rejects omitting the default log level" $ do
      readLogLevels "foo:warn" `shouldSatisfy` isLeft

    it "rejects multiple default log levels" $ do
      readLogLevels "info,warn" `shouldSatisfy` isLeft

    it "rejects multiple default log levels with sources" $ do
      readLogLevels "info,foo:warn,info" `shouldSatisfy` isLeft

    it "rejects invalid sources" $ do
      readLogLevels "info,foo:" `shouldSatisfy` isLeft
      readLogLevels "info,:bar" `shouldSatisfy` isLeft

    it "accepts the default log level in any position" $ do
      readLogLevels "info,foo:warn" `shouldSatisfy` isRight
      readLogLevels "foo:warn,info" `shouldSatisfy` isRight
      readLogLevels "foo:warn,info,foo:warn" `shouldSatisfy` isRight

  describe "showLogLevels" $ do
    it "shows a simple log levels" $ do
      let ll = newLogLevels LevelWarn []

      showLogLevels ll `shouldBe` "warn"

    it "shows complex log levels in normalized order and casing" $ do
      -- Using readLogLevel here so we can highlight how it gets normalized
      let ell = readLogLevels "foo:WARN,info,bar:debug"

      fmap showLogLevels ell `shouldBe` Right "info,bar:debug,foo:warn"

    -- This is a compromise vs using a proper property because it was just too
    -- complex to make Arbitrary LogLevels
    it "round-trips through readLogLevels" $ do
      let ll = newLogLevels LevelWarn [("foo", LevelInfo), ("bar", LevelDebug)]

      readLogLevels (showLogLevels ll) `shouldBe` Right ll

  describe "shouldLogLevel" $ do
    it "uses the default log level for unknown sources" $ do
      let
        ll1 = newLogLevels LevelWarn []
        ll2 = newLogLevels LevelWarn [("foo", LevelDebug)]

      shouldLogLevel ll1 "foo" LevelInfo `shouldBe` False
      shouldLogLevel ll1 "bar" LevelInfo `shouldBe` False

      shouldLogLevel ll2 "foo" LevelInfo `shouldBe` True
      shouldLogLevel ll2 "bar" LevelInfo `shouldBe` False

    it "can override multiple sources" $ do
      let ll =
            newLogLevels LevelDebug [("Amazonka", LevelWarn), ("SQL", LevelInfo)]

      shouldLogLevel ll "app" LevelDebug `shouldBe` True
      shouldLogLevel ll "Amazonka" LevelInfo `shouldBe` False
      shouldLogLevel ll "SQL" LevelDebug `shouldBe` False

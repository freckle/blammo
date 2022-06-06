{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

  describe "shouldLogLevel" $ do
    it "uses the default log level for unknown sources" $ do
      let
        Right ll1 = readLogLevels "warn"
        Right ll2 = readLogLevels "warn,foo:debug"

      shouldLogLevel ll1 "foo" LevelInfo `shouldBe` False
      shouldLogLevel ll1 "bar" LevelInfo `shouldBe` False

      shouldLogLevel ll2 "foo" LevelInfo `shouldBe` True
      shouldLogLevel ll2 "bar" LevelInfo `shouldBe` False

    it "can override multiple sources" $ do
      let Right ll = readLogLevels "debug,Amazonka:warn,SQL:info"

      shouldLogLevel ll "app" LevelDebug `shouldBe` True
      shouldLogLevel ll "Amazonka" LevelInfo `shouldBe` False
      shouldLogLevel ll "SQL" LevelDebug `shouldBe` False

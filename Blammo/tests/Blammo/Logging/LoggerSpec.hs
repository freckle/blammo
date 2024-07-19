{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Blammo.Logging.LoggerSpec
  ( spec
  ) where

import Prelude

import Blammo.Logging
import Blammo.Logging.Logger
import Control.Monad.Reader (runReaderT)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Compat as KeyMap
import Data.Text (Text)
import Test.Hspec

spec :: Spec
spec = do
  describe "newTestLogger" $ do
    it "it captures messages to be accessed later" $ do
      [Right m1, Right m2] <- do
        logger <- newTestLogger defaultLogSettings
        runLoggerLoggingT logger $ flip runReaderT logger $ do
          logInfo "Hello"
          logWarn $ "World" :# ["with" .= ("data" :: Text)]
          getLoggedMessages

      loggedMessageLevel m1 `shouldBe` LevelInfo
      loggedMessageText m1 `shouldBe` "Hello"
      loggedMessageMeta m1 `shouldBe` KeyMap.empty

      loggedMessageLevel m2 `shouldBe` LevelWarn
      loggedMessageText m2 `shouldBe` "World"
      loggedMessageMeta m2 `shouldBe` KeyMap.fromList [("with", String "data")]

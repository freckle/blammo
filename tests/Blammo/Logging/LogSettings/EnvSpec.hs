module Blammo.Logging.LogSettings.EnvSpec
  ( spec
  ) where

import Prelude

import Blammo.Logging.LogSettings
import qualified Blammo.Logging.LogSettings.Env as LogSettingsEnv
import Blammo.Logging.LogSettings.LogLevels
import Data.List (intercalate)
import qualified Env
import Test.Hspec

spec :: Spec
spec = do
  context "LOG_LEVEL" $ do
    it "reads a simple log-level" $ do
      let env = [("LOG_LEVEL", "debug")]

      settings <- parseLogSettings env
      getLogSettingsLevels settings `shouldBe` newLogLevels LevelDebug []

  context "LOG_FORMAT and LOG_CONCURRENCY" $ do
    it "Sets LOG_CONCURRENCY if format is tty" $ do
      settings <- parseLogSettings []
      settingsTTY <- parseLogSettings [("LOG_FORMAT", "tty")]

      getLogSettingsConcurrency settings `shouldBe` Just 1
      getLogSettingsConcurrency settingsTTY `shouldBe` Just 1

    it "Unsets LOG_CONCURRENCY if format is not tty" $ do
      settings <- parseLogSettings []
      settingsJSON <- parseLogSettings [("LOG_FORMAT", "json")]

      getLogSettingsConcurrency settings `shouldBe` Just 1
      getLogSettingsConcurrency settingsJSON `shouldBe` Nothing

    it "Respects explicit LOG_CONCURRENCY" $ do
      settingsTTY <- parseLogSettings [("LOG_FORMAT", "tty"), ("LOG_CONCURRENCY", "2")]
      settingsJSON <- parseLogSettings [("LOG_FORMAT", "json"), ("LOG_CONCURRENCY", "3")]

      getLogSettingsConcurrency settingsTTY `shouldBe` Just 2
      getLogSettingsConcurrency settingsJSON `shouldBe` Just 3

  context "NO_COLOR" $ do
    it "changes LOG_COLOR to never" $ do
      settings <- parseLogSettings []
      settingsC <- parseLogSettings [("NO_COLOR", "")]
      settingsNC <- parseLogSettings [("NO_COLOR", "x")]

      getLogSettingsColor settings `shouldBe` LogColorAuto
      getLogSettingsColor settingsC `shouldBe` LogColorAuto
      getLogSettingsColor settingsNC `shouldBe` LogColorNever

    it "respects explicit LOG_COLOR after" $ do
      settings <- parseLogSettings [("NO_COLOR", "x"), ("LOG_COLOR", "always")]

      getLogSettingsColor settings `shouldBe` LogColorAlways

  context "TERM=dumb" $ do
    it "changes LOG_COLOR to never" $ do
      settings <- parseLogSettings []
      settingsC <- parseLogSettings [("TERM", "xterm")]
      settingsNC <- parseLogSettings [("TERM", "dumb")]

      getLogSettingsColor settings `shouldBe` LogColorAuto
      getLogSettingsColor settingsC `shouldBe` LogColorAuto
      getLogSettingsColor settingsNC `shouldBe` LogColorNever

    it "respects explicit LOG_COLOR after" $ do
      settings <- parseLogSettings [("TERM", "dumb"), ("LOG_COLOR", "always")]

      getLogSettingsColor settings `shouldBe` LogColorAlways

-- context "TERM=dumb" $ do

parseLogSettings :: [(String, String)] -> IO LogSettings
parseLogSettings env = do
  case Env.parsePure LogSettingsEnv.parser env of
    Left es -> expectationFailure (failureMessage es) *> error "Unreachable"
    Right s -> pure s
 where
  failureMessage :: [(String, Env.Error)] -> String
  failureMessage =
    ("Expected parse to succeed, but there were errors:\n" <>)
      . intercalate "\n  "
      . map showEnvError

  showEnvError :: (String, Env.Error) -> String
  showEnvError (name, e) =
    name <> " :" <> case e of
      Env.UnsetError -> "expected, but not set"
      Env.EmptyError -> "expected, but was empty"
      Env.UnreadError x -> "invalid: " <> show x

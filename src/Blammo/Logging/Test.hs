{-# LANGUAGE CPP #-}

module Blammo.Logging.Test
  ( LoggedMessages
  , LoggedMessage(..)
  , newLoggedMessages
  , appendLogStr
  , getLoggedMessages
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger.Aeson (LoggedMessage(..))
import Data.Aeson (eitherDecodeStrict)
import Data.DList (DList)
import qualified Data.DList as DList
import System.Log.FastLogger (LogStr, fromLogStr)
import UnliftIO.IORef

newtype LoggedMessages = LoggedMessages
  { _unLoggedMessages :: IORef (DList LogStr)
  }

newLoggedMessages :: MonadIO m => m LoggedMessages
newLoggedMessages = LoggedMessages <$> newIORef DList.empty

appendLogStr :: MonadIO m => LoggedMessages -> LogStr -> m ()
appendLogStr (LoggedMessages ref) str =
  atomicModifyIORef' ref $ \x -> (DList.snoc x str, ())

getLoggedMessages :: MonadIO m => LoggedMessages -> m [Either String LoggedMessage]
getLoggedMessages (LoggedMessages ref) =
  map (eitherDecodeStrict . fromLogStr) . DList.toList <$> readIORef ref

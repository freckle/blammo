{-# LANGUAGE CPP #-}

module System.Log.FastLogger.Compat
  ( newFileLoggerSetN
  , newStdoutLoggerSetN
  , newStderrLoggerSetN
  ) where

import System.Log.FastLogger

#if !MIN_VERSION_fast_logger(3, 1, 1)
import Prelude

newStdoutLoggerSetN :: BufSize -> Maybe Int -> IO LoggerSet
newStdoutLoggerSetN size _ = newStdoutLoggerSet size

newStderrLoggerSetN :: BufSize -> Maybe Int -> IO LoggerSet
newStderrLoggerSetN size _ = newStderrLoggerSet size

#if !MIN_VERSION_fast_logger(3, 0, 5)
newFileLoggerSetN :: BufSize -> Maybe Int -> FilePath -> IO LoggerSet
newFileLoggerSetN size _ = newFileLoggerSet size
#endif
#endif

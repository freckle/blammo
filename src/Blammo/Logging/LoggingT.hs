{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Blammo.Logging.LoggingT (LoggingT (..), runLoggerLoggingT) where

import Prelude

import Blammo.Logging.Internal.LogAction
import Blammo.Logging.Internal.LoggerLogAction (loggerLogAction)
import Blammo.Logging.Logger
import Control.Applicative (Alternative (..), Applicative (..))
import Control.Lens ((^.))
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Catch (MonadCatch (..), MonadMask (..), MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger.Aeson
  ( MonadLogger (..)
  , MonadLoggerIO (..)
  )
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Control
  ( MonadBaseControl (..)
  , MonadTransControl (..)
  )
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Resource (MonadResource (..))
import UnliftIO.Exception (finally)

newtype LoggingT m a = LoggingT {runLoggingT :: Logger -> m a}
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadFail
    , MonadIO
    , MonadUnliftIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadResource
    )
    via ReaderT Logger m
  deriving
    ( MonadTrans
    )
    via ReaderT Logger

instance MonadBase b m => MonadBase b (LoggingT m) where
  liftBase = lift . liftBase

instance MonadTransControl LoggingT where
  type StT LoggingT a = a
  liftWith f = LoggingT $ \r -> f $ \(LoggingT t) -> t r
  restoreT = LoggingT . const

instance MonadBaseControl b m => MonadBaseControl b (LoggingT m) where
  type StM (LoggingT m) a = StM m a
  liftBaseWith f = LoggingT $ \reader' ->
    liftBaseWith $ \runInBase ->
      f $ runInBase . (\(LoggingT r) -> r reader')
  restoreM = LoggingT . const . restoreM

instance MonadIO m => MonadLogger (LoggingT m) where
  monadLoggerLog a b c d = LoggingT $ \logger ->
    liftIO $ runLogAction (loggerLogAction logger) a b c d

instance MonadIO m => MonadLoggerIO (LoggingT m) where
  askLoggerIO = LoggingT $ \logger ->
    pure $ runLogAction $ loggerLogAction logger

instance (Applicative m, Semigroup a) => Semigroup (LoggingT m a) where
  (<>) = liftA2 (<>)

instance (Applicative m, Monoid a) => Monoid (LoggingT m a) where
  mempty = pure mempty

runLoggerLoggingT
  :: (MonadUnliftIO m, HasLogger env) => env -> LoggingT m a -> m a
runLoggerLoggingT env f =
  runLoggingT f logger `finally` flushLogStr logger
 where
  logger = env ^. loggerL

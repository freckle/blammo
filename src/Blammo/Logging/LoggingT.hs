{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Blammo.Logging.LoggingT (LoggingT, LoggingT' (..), runLoggerLoggingT) where

import Prelude

#if MIN_VERSION_base(4, 19, 0)
#else
import Control.Applicative (Applicative (..))
#endif

import Blammo.Logging.Internal.LogAction
import Blammo.Logging.Internal.LoggerLogAction (loggerLogAction)
import Blammo.Logging.Logger
import Control.Applicative (Alternative (..))
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

type LoggingT = LoggingT' Logger

newtype LoggingT' env m a = LoggingT {runLoggingT :: env -> m a}
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
    via ReaderT env m
  deriving
    ( MonadTrans
    )
    via ReaderT env

instance MonadBase b m => MonadBase b (LoggingT' env m) where
  liftBase = lift . liftBase

instance MonadTransControl (LoggingT' env) where
  type StT (LoggingT' env) a = a
  liftWith f = LoggingT $ \r -> f $ \(LoggingT t) -> t r
  restoreT = LoggingT . const

instance MonadBaseControl b m => MonadBaseControl b (LoggingT' env m) where
  type StM (LoggingT' env m) a = StM m a
  liftBaseWith f = LoggingT $ \reader' ->
    liftBaseWith $ \runInBase ->
      f $ runInBase . (\(LoggingT r) -> r reader')
  restoreM = LoggingT . const . restoreM

instance (MonadIO m, HasLogger env) => MonadLogger (LoggingT' env m) where
  monadLoggerLog a b c d = do
    logger <- getLogger
    liftIO $ runLogAction (loggerLogAction logger) a b c d

instance (MonadIO m, HasLogger env) => MonadLoggerIO (LoggingT' env m) where
  askLoggerIO =
    runLogAction . loggerLogAction <$> getLogger

instance (Applicative m, Semigroup a) => Semigroup (LoggingT' env m a) where
  (<>) = liftA2 (<>)

instance (Applicative m, Monoid a) => Monoid (LoggingT' env m a) where
  mempty = pure mempty

runLoggerLoggingT
  :: (MonadUnliftIO m, HasLogger env) => env -> LoggingT m a -> m a
runLoggerLoggingT env f =
  runLoggingT f logger `finally` flushLogStr logger
 where
  logger = env ^. loggerL

getLogger :: (Applicative m, HasLogger env) => LoggingT' env m Logger
getLogger = LoggingT $ pure . (^. loggerL)

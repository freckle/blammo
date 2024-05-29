{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Blammo.Logging.LoggingT (LoggingT (..), filterLogger) where

import Prelude

import Blammo.Logging.Internal.LogAction
import Control.Applicative (Alternative (..), Applicative (..))
import Control.Monad (when)
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Catch (MonadCatch (..), MonadMask (..), MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Logger.Aeson
  ( LogLevel
  , LogSource
  , MonadLogger (..)
  , MonadLoggerIO (..)
  )
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Control
  ( MonadBaseControl (..)
  , MonadTransControl (..)
  )
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Resource (MonadResource (..))

newtype LoggingT m a = LoggingT {runLoggingT :: LogAction IO -> m a}
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
    via ReaderT (LogAction IO) m
  deriving
    ( MonadTrans
    )
    via ReaderT (LogAction IO)

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
  monadLoggerLog a b c d = LoggingT $ \f -> liftIO $ runLogAction f a b c d

instance MonadIO m => MonadLoggerIO (LoggingT m) where
  askLoggerIO = LoggingT $ \(LogAction f) -> pure f

instance (Applicative m, Semigroup a) => Semigroup (LoggingT m a) where
  (<>) = liftA2 (<>)

instance (Applicative m, Monoid a) => Monoid (LoggingT m a) where
  mempty = pure mempty

-- | Only log messages passing the given predicate function
--
-- This can be a convenient way, for example, to ignore debug level messages.
filterLogger
  :: (LogSource -> LogLevel -> Bool)
  -> LoggingT m a
  -> LoggingT m a
filterLogger p (LoggingT f) = LoggingT $ \(LogAction logger) ->
  f $ LogAction $ \loc src level msg ->
    when (p src level) $ logger loc src level msg

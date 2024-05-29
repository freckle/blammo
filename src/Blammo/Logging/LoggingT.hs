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
import Control.Monad.Logger.Aeson hiding (LoggingT (..), filterLogger)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Control
  ( MonadBaseControl (..)
  , MonadTransControl (..)
  )
import Control.Monad.Trans.Resource (MonadResource (..))

newtype LoggingT m a = LoggingT {runLoggingT :: LogAction IO -> m a}

instance Functor m => Functor (LoggingT m) where
  fmap f logger = LoggingT (fmap f . runLoggingT logger)

instance Applicative m => Applicative (LoggingT m) where
  pure = LoggingT . const . pure
  loggerF <*> loggerA = LoggingT $ \loggerFn ->
    runLoggingT loggerF loggerFn
      <*> runLoggingT loggerA loggerFn

instance Alternative m => Alternative (LoggingT m) where
  empty = LoggingT (const empty)
  LoggingT x <|> LoggingT y = LoggingT (\f -> x f <|> y f)

instance Monad m => Monad (LoggingT m) where
  LoggingT ma >>= f = LoggingT $ \r -> do
    a <- ma r
    let LoggingT f' = f a
    f' r

instance MonadIO m => MonadIO (LoggingT m) where
  liftIO = lift . liftIO

instance MonadThrow m => MonadThrow (LoggingT m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (LoggingT m) where
  catch (LoggingT m) c =
    LoggingT $ \r -> m r `catch` \e -> runLoggingT (c e) r

instance MonadMask m => MonadMask (LoggingT m) where
  mask a = LoggingT $ \e -> mask $ \u -> runLoggingT (a $ q u) e
   where
    q u (LoggingT b) = LoggingT (u . b)
  uninterruptibleMask a =
    LoggingT $ \e -> uninterruptibleMask $ \u -> runLoggingT (a $ q u) e
   where
    q u (LoggingT b) = LoggingT (u . b)
  generalBracket acquire release use =
    LoggingT $ \e ->
      generalBracket
        (runLoggingT acquire e)
        (\x ec -> runLoggingT (release x ec) e)
        (\x -> runLoggingT (use x) e)

instance MonadResource m => MonadResource (LoggingT m) where
  liftResourceT = lift . liftResourceT

instance MonadBase b m => MonadBase b (LoggingT m) where
  liftBase = lift . liftBase

instance MonadTrans LoggingT where
  lift = LoggingT . const

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

instance MonadUnliftIO m => MonadUnliftIO (LoggingT m) where
  withRunInIO inner =
    LoggingT $ \r ->
      withRunInIO $ \run ->
        inner (run . flip runLoggingT r)

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

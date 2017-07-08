{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tracker.Handlers where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Config
import           Data.Conduit
import           Tracker.State
import           Tracker.Types

newtype HandlerM a
  = HandlerM (ExceptT Failure (ReaderT Config (StateT LocalState IO)) a)
  deriving (Functor, Applicative, Monad, MonadState LocalState,
            MonadReader Config, MonadError Failure, MonadIO)

type HandlerMonad m = (MonadState LocalState m, MonadReader Config m,
                       MonadError Failure m, MonadIO m)

-- |Run an action in the HandlerM monad with configuration `cfg` and initial
-- state `st`.
runHandlerM :: HandlerM a -> Config -> LocalState -> IO (Either Failure a, LocalState)
runHandlerM (HandlerM action) cfg =
  runStateT (runReaderT (runExceptT action) cfg)

-- |Search JIRA for issues matching the JQL query.
search :: HandlerMonad m => JQL -> Source m [Issue]
search = undefined

-- |Start work on the issue at specified time.
start :: HandlerMonad m => IssueKey -> Bool -> Maybe Timestamp -> m ()
start key check Nothing = getCurrentTimestamp >>= startWithTimestamp key check
start key check (Just ts) = startWithTimestamp key check ts

startWithTimestamp :: HandlerMonad m => IssueKey -> Bool -> Timestamp -> m ()
startWithTimestamp = undefined

-- |Stop working on the active issue at specified time.
stop :: HandlerMonad m => Maybe Timestamp -> m ()
stop Nothing   = getCurrentTimestamp >>= stopWithTimestamp
stop (Just ts) = stopWithTimestamp ts

book :: HandlerM ()
book = undefined

stopWithTimestamp :: HandlerMonad m => Timestamp -> m ()
stopWithTimestamp ts = tryAppendEvent (Stopped ts)

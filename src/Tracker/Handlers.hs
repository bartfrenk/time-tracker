{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Tracker.Handlers where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State

import           Backend
import           Config
import           Data.Conduit
import           Tracker.State
import           Tracker.Types

type TrackerMonad m = (MonadState LocalState m,
                       MonadReader Config m,
                       MonadBackend m,
                       MonadThrow m)

-- |Search JIRA for issues matching the JQL query.
search :: (MonadReader Config m, MonadBackend m)
       => JQL -> Source m [Issue]
search = undefined

start :: (MonadBackend m, MonadReader Config m, MonadState LocalState m, MonadThrow m)
      => IssueKey -> Timestamp -> m ()
start = undefined

stop :: (MonadState LocalState m, MonadThrow m)
     => Timestamp -> m ()
stop ts = appendEvent (Stopped ts)

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Tracker where

import           BasicPrelude
import           Control.Concurrent.MVar
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import           Data.Conduit
import           Data.Tuple              (swap)
import           GHC.Generics

import qualified Backend
import           Tracker.State
import           Tracker.Types as Tracker


withHandle :: Tracker.Config -> Backend.Handle -> (Tracker.Handle -> IO a) -> IO a
withHandle config backend cont = do
  initState <- loadState $ statePath config
  stateVar <- newMVar initState
  let h = Tracker.Handle
        { start = \partialKey time ->
            let act = runReaderT (startM backend partialKey time) config
            in  modifyState stateVar act
        , stop = \time ->
            let act = runReaderT (stopM time) config
            in modifyState stateVar act
        , search = \jql sink ->
            runReaderT (searchM backend jql sink) config
        , review = \time ->
            let act = runReaderT (reviewM time) config
            in evalState act <$> readMVar stateVar
        }
  result <- cont h
  finalState <- readMVar stateVar
  unless (finalState == initState) $ saveState (statePath config) finalState
  return result

modifyState :: MVar s -> StateT s IO a -> IO a
modifyState stateVar act =
  modifyMVar stateVar ((swap `fmap`) <$> runStateT act)

data Handle = Handle
  { search :: JQL -> Sink Issue IO () -> IO ()
  , start  :: PartialIssueKey -> Timestamp -> IO Issue
  , stop   :: Timestamp -> IO (Maybe LogItem)
  , review :: Timestamp -> IO ([LogItem], Maybe LogItem)
  }

-- |Search JIRA for issues matching the JQL query.
searchM :: (MonadIO m, MonadReader Tracker.Config m)
        => Backend.Handle -> JQL -> Sink Issue IO () -> m ()
searchM backend jql sink =
  liftIO $ runConduit (searchConduit backend jql $= sink)

searchConduit :: Backend.Handle -> JQL -> Source IO Issue
searchConduit backend jql = loop 0
  where
    fetch = Backend.search backend jql
    limit = 2
    loop offset = do
      issues <- lift $ fetch (Backend.Page offset limit)
      mapM_ yield issues
      unless (null issues) $
        loop (offset + length issues)

startM :: (MonadReader Tracker.Config m, MonadState LocalState m, MonadThrow m, MonadIO m)
       => Backend.Handle -> PartialIssueKey -> Timestamp -> m Issue
startM backend partialKey time = do
  defaultProject <- reader defaultProject
  key <- completeToIssueKey defaultProject partialKey
  issue' <- liftIO $ Backend.fetch backend key
  case issue' of
    Nothing -> throwM $ IssueNotFound key
    Just issue -> do
      appendEvent (Started time key)
      return issue

stopM :: (MonadState LocalState m, MonadThrow m)
      => Timestamp -> m (Maybe LogItem)
stopM ts = do
  appendEvent (Stopped ts)
  readLastLogItem

reviewM :: (MonadState LocalState m, MonadReader Tracker.Config m)
        => Timestamp -> m ([LogItem], Maybe LogItem)
reviewM ts = do
  logItems <- gets (snd . takeAllLogItems)
  activeLogItem <- readActiveLogItem ts
  return (logItems, activeLogItem)

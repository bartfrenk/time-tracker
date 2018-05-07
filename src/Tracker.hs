{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}

module Tracker
  ( module Tracker
  ) where

import           BasicPrelude            hiding (try)
import           Control.Concurrent.MVar
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Conduit
import           Data.Conduit.Lift
import qualified Data.Map.Strict         as M
import           Data.Tuple              (swap)

import qualified Backend
import           Shared.Types            (getTimestamp)
import           Shared.Utils            (expandFilePath)
import           Tracker.State
import           Tracker.Types           as Tracker

withHandle ::
     Tracker.Config -> Backend.Handle -> (Tracker.Handle -> IO a) -> IO a
withHandle config backend cont = do
  expandedStatePath <- expandFilePath (statePath config)
  initState <- loadState expandedStatePath
  stateVar <- newMVar initState
  let h =
        Tracker.Handle
        { start =
            \partialKey time ->
              let act = runReaderT (startM backend partialKey time) config
              in modifyState stateVar act
        , stop =
            \time ->
              let act = runReaderT (stopM time) config
              in modifyState stateVar act
        , search = \jql sink -> runReaderT (searchM backend jql sink) config
        , status =
            \time ->
              let act = runReaderT (statusM time) config
              in evalState act <$> readMVar stateVar
        , book =
            \sink ->
              let src = runReaderC config (bookM backend)
              in modifyStateC stateVar src sink
        , close =
            let act = runReaderT closeM config
            in modifyState stateVar act
        }
  result <- cont h
  finalState <- readMVar stateVar
  unless (finalState == initState) $ saveState expandedStatePath finalState
  return result

-- |Atomically modifies the MVar by the state action.
modifyStateC :: MVar s -> Source (StateT s IO) r -> Sink r IO () -> IO ()
modifyStateC stateVar src sink =
  let f st = runConduit (runStateLC st src `fuseUpstream` sink)
  in modifyMVar stateVar ((swap `fmap`) <$> f)

-- |Atomically modifies the MVar by the state action.
modifyState :: MVar s -> StateT s IO a -> IO a
modifyState stateVar act = modifyMVar stateVar ((swap `fmap`) <$> runStateT act)

-- |Interface for the Tracker service.
data Handle = Handle
  { search :: Text -> Sink Issue IO () -> IO ()
  , start  :: PartialIssueKey -> Timestamp -> IO Issue
  , stop   :: Timestamp -> IO (Maybe LogItem)
  , status :: Timestamp -> IO ([LogItem], Maybe LogItem)
  , book   :: Sink BookResult IO () -> IO ()
  , close  :: IO [Event]
  }

-- |Search JIRA for issues matching the JQL query.
searchM ::
     (MonadIO m, MonadReader Tracker.Config m)
  => Backend.Handle
  -> Text
  -> Sink Issue IO ()
  -> m ()
searchM backend query sink = do
  jql <- expandQuery query
  liftIO $ runConduit (searchConduit backend jql $= sink)

-- |Looks up the key as a possible query alias, if none is found, returns the key
-- itself as a JQL query.
expandQuery :: MonadReader Tracker.Config m => Text -> m JQL
expandQuery key = do
  aliases <- reader queries
  return $ M.findWithDefault (JQL key) key aliases

-- |Returns a source of issues that match the specified query.
searchConduit :: Backend.Handle -> JQL -> Source IO Issue
searchConduit backend jql = loop 0
  where
    fetch = Backend.search backend jql
    limit = 1000
    loop offset = do
      fetchedIssues <- lift $ fetch (Backend.Page offset limit)
      mapM_ yield fetchedIssues
      unless (null fetchedIssues) $ loop (offset + length fetchedIssues)

startM ::
     ( MonadReader Tracker.Config m
     , MonadState LocalState m
     , MonadThrow m
     , MonadIO m
     )
  => Backend.Handle
  -> PartialIssueKey
  -> Timestamp
  -> m Issue
startM backend partialKey time = do
  key <- completeToIssueKey partialKey
  issue' <- liftIO $ Backend.fetch backend key
  case issue' of
    Nothing -> throwM $ IssueNotFound key
    Just issue -> do
      appendEvent (Started time key)
      return issue

stopM ::
     (MonadState LocalState m, MonadThrow m) => Timestamp -> m (Maybe LogItem)
stopM ts = do
  appendEvent (Stopped ts)
  readLastLogItem

statusM ::
     (MonadState LocalState m, MonadReader Tracker.Config m)
  => Timestamp
  -> m ([LogItem], Maybe LogItem)
statusM ts = do
  logItems <- gets (snd . takeAllLogItems)
  activeLogItem <- readActiveLogItem ts
  return (logItems, activeLogItem)

closeM ::
     (MonadIO m, MonadState LocalState m, MonadReader Tracker.Config m)
  => m [Event]
closeM = do
  ts <- getTimestamp
  reader stopAt >>= closeForgotten ts

-- |The result of booking a work log item to a backend. Constructors `Booked`
-- and `Failed` are straightforward. A result of `Discarded` means that the log
-- item was not sent, for example, when less time on it was spent then required
-- for the backend.
data BookResult
  = Booked LogItem
  | Discarded LogItem
  | Failed SomeException
  deriving (Show)

-- |Books all locally outstanding work log items via the backend (e.g. JIRA),
-- and returns a source in which the result of each individual booking is placed.
bookM ::
     ( MonadState LocalState m
     , MonadReader Tracker.Config m
     , MonadIO m
     , MonadCatch m
     )
  => Backend.Handle
  -> Source m BookResult
bookM backend@Backend.Handle {..} = do
  (rest, logItem') <- takeLogItem <$> get
  case logItem' of
    Just logItem ->
      if canBeBooked logItem
        then do
          result <- try (liftIO $ book logItem)
          case result of
            Left exc -> yield $ Failed exc
            Right _ -> do
              put rest
              yield (Booked logItem)
              bookM backend
        else do
          put rest
          yield (Discarded logItem)
          bookM backend
    Nothing -> do
      put rest
      return ()

canBeBooked :: LogItem -> Bool
canBeBooked LogItem {..} = toSeconds timeSpent >= 60

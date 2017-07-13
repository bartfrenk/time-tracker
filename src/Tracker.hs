{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tracker where

import           Control.Concurrent.MVar
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Conduit
import           Data.Tuple              (swap)

import qualified Backend
import           Tracker.State
import           Tracker.Types


withHandle :: Config -> Backend.Handle -> (Tracker.Handle -> IO a) -> IO a
withHandle config backend cont = do
  initState <- loadState $ statePath config
  stateVar <- newMVar initState
  let h = Tracker.Handle
        { start = \issue time ->
            let act = runReaderT (startM backend issue time) config
            in  modifyState stateVar act
        , stop = \time ->
            let act = runReaderT (stopM time) config
            in modifyState stateVar act
        , search = \jql sink ->
            runReaderT (searchM backend jql sink) config
        }
  result <- cont h
  final <- readMVar stateVar
  saveState (statePath config) final
  return result

modifyState :: MVar s -> StateT s IO a -> IO a
modifyState stateVar act =
  modifyMVar stateVar ((swap `fmap`) <$> runStateT act)

data Config = Config
  {
    statePath :: FilePath
  }

data Handle = Handle
  { search :: JQL -> Sink Issue IO () -> IO ()
  , start  :: IssueKey -> Timestamp -> IO ()
  , stop   :: Timestamp -> IO ()
  }

-- |Search JIRA for issues matching the JQL query.
searchM :: (MonadIO m, MonadReader Config m)
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


startM :: (MonadReader Config m, MonadState LocalState m, MonadThrow m)
       => Backend.Handle -> IssueKey -> Timestamp -> m ()
startM = undefined

stopM :: (MonadState LocalState m, MonadThrow m)
      => Timestamp -> m ()
stopM ts = appendEvent (Stopped ts)

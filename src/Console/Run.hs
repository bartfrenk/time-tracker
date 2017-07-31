{-# LANGUAGE FlexibleContexts #-}
module Console.Run where

import           Control.Monad.Trans (liftIO, MonadIO)
import           Data.Version        (showVersion)

import           Paths_time_tracker        (version)

import           Tracker
import Tracker.Types


printVersion :: MonadIO m => m ()
printVersion =
  liftIO $ putStrLn (showVersion version)

startIssue :: MonadIO m => Tracker.Handle -> IssueKey -> Timestamp -> m ()
startIssue tracker key ts = liftIO $ do
  issue <- Tracker.start tracker key ts
  print issue

stopIssue :: MonadIO m => Tracker.Handle -> Timestamp -> m ()
stopIssue tracker ts = liftIO $ do
  item' <- Tracker.stop tracker ts
  case item' of
    Just item -> print item
    Nothing -> return ()

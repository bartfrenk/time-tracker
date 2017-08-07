{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Console.Run where

import           BasicPrelude
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Text           as T
import           Data.Version        (showVersion)

import           Paths_time_tracker  (version)

import           Console.Format
import           Shared.Types
import           Tracker
import           Tracker.Types


printVersion :: MonadIO m => m ()
printVersion =
  liftIO $ putStrLn (T.pack $ showVersion version)

startIssue :: MonadIO m => Tracker.Handle -> PartialIssueKey -> Timestamp -> m ()
startIssue tracker partialKey ts = liftIO $ do
  issue <- Tracker.start tracker partialKey ts
  print issue

stopIssue :: MonadIO m => Tracker.Handle -> Timestamp -> m ()
stopIssue tracker ts = liftIO $ do
  item' <- Tracker.stop tracker ts
  case item' of
    Just item -> print item
    Nothing   -> return ()

review :: MonadIO m => Tracker.Handle -> Timestamp -> m ()
review tracker ts = liftIO $ Tracker.review tracker ts >>= printReview

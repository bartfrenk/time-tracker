{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Console.Run where

import           BasicPrelude
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Conduit.List        as CL
import qualified Data.Text           as T
import           Data.Version        (showVersion)

import           Paths_time_tracker  (version)

import           Console.Format
import           Shared.Types
import           Tracker


printVersion :: MonadIO m => m ()
printVersion =
  liftIO $ putStrLn (T.pack $ showVersion version)

startIssue :: MonadIO m => Tracker.Handle -> PartialIssueKey -> Timestamp -> m ()
startIssue tracker partialKey ts = liftIO $
  Tracker.start tracker partialKey ts >>= printIssue

stopIssue :: MonadIO m => Tracker.Handle -> Timestamp -> m ()
stopIssue tracker ts = liftIO $ do
  item' <- Tracker.stop tracker ts
  case item' of
    Just item -> print item
    Nothing   -> return ()

review :: MonadIO m => Tracker.Handle -> Timestamp -> m ()
review tracker ts = liftIO $ Tracker.review tracker ts >>= printReview

book :: MonadIO m => Tracker.Handle -> m ()
book tracker = liftIO $ Tracker.book tracker printer
  where printer = CL.mapM_ print

search :: MonadIO m => Tracker.Handle -> Text -> m ()
search tracker query = liftIO $ Tracker.search tracker query printer
  where printer = CL.mapM_ printIssue

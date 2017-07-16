{-# LANGUAGE NoImplicitPrelude #-}
module Console.Args where

import           BasicPrelude
import           Control.Monad.Trans (MonadIO)

import           Console.Config
import           Tracker.Types

data Command
  = Search JQL
  | Start IssueKey Timestamp
  | Stop Timestamp
  | Review
  | Book
  | Version
  deriving (Eq, Show)

runParser :: MonadIO m => Config -> [Text] -> m Command
runParser config args = return Version

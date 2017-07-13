{-# LANGUAGE FlexibleContexts #-}
module Console.Run where

import           Control.Monad.Trans (liftIO, MonadIO)
import           Data.Version        (showVersion)

import           Paths_time_tracker        (version)

import           Tracker


printVersion :: MonadIO m => m ()
printVersion =
  liftIO $ putStrLn (showVersion version)



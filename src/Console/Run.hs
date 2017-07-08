{-# LANGUAGE FlexibleContexts #-}
module Console.Run where

import           Control.Monad.Trans (liftIO)
import           Data.Version        (showVersion)

import           Paths_time_tracker        (version)

import           Tracker.Handlers


printVersion :: HandlerMonad m => m ()
printVersion =
  liftIO $ putStrLn (showVersion version)



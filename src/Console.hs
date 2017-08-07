{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Console where

import           BasicPrelude
import           Control.Monad.Reader


import           Console.Args
import           Console.Config
import           Console.Run
import           Shared.Types
import           Shared.Utils         (expand)
import qualified Tracker

data Handle = Handle
  { run :: [Text] -> IO ()
  }

withHandle :: Config -> Tracker.Handle -> (Console.Handle -> IO a) -> IO a
withHandle config tracker cont = cont (newHandle config tracker)

newHandle :: Config -> Tracker.Handle -> Console.Handle
newHandle config tracker = Console.Handle
  { run = \args -> runReaderT (runM tracker args) config
  }

runM :: (MonadIO m, MonadReader Config m)
     => Tracker.Handle -> [Text] -> m ()
runM tracker args = do
  config <- ask
  command <- runParser config (expand (aliases config) args)
  process tracker command

process :: MonadIO m
        => Tracker.Handle -> Command -> m ()
process _ Version                     = printVersion
process tracker (Start partialKey ts) = startIssue tracker partialKey ts
process tracker (Stop ts)             = stopIssue tracker ts
process tracker Review                = review tracker =<< getTimestamp

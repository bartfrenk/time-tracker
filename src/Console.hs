{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Console where

import           BasicPrelude
import           Control.Monad.Reader


import           Console.Args
import           Console.Config
import           Console.Run
import           Shared.Utils         (expand)
import           Tracker              (Handle)

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

process :: (MonadReader Config m, MonadIO m)
        => Tracker.Handle -> Command -> m ()
process _ Version = printVersion

-- newtype ConsoleM a =
--   ConsoleM (ReaderT Config (StateT LocalState IO) a)
--   deriving (Functor, Applicative, Monad, MonadReader Config, MonadState LocalState,
--             MonadIO)

-- withClientEnv :: (MonadReader Config m) => ReaderT JIRA.ClientEnv m a -> m a
-- withClientEnv act = do
--   env <- getClientEnv <$> ask
--   JIRA.runClientT act env
--   where getClientEnv = undefined

-- instance MonadThrow ConsoleM where
--   throwM = liftIO . throwIO

-- instance MonadBackend ConsoleM where
--   book item = runReaderT $ JIRA.bookM item
--   search jql page = withClientEnv $ JIRA.searchM jql page

-- run :: [String] -> ConsoleM ()
-- run args = do
--   config <- ask
--   command <- runParser config args
--   process command
--   where
--     process Version = printVersion

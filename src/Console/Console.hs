{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Console where

import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State

import           Backend

import qualified Backend.Impl.JIRA    as JIRA
import           Config
import           Console.ArgParser
import           Console.Run
import           Tracker.Handlers
import           Tracker.State

newtype ConsoleM a =
  ConsoleM (ReaderT Config (StateT LocalState IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState LocalState,
            MonadIO)

withClientEnv :: (MonadReader Config m) => ReaderT JIRA.ClientEnv m a -> m a
withClientEnv act = do
  env <- getClientEnv <$> ask
  JIRA.runClientT act env
  where getClientEnv = undefined

instance MonadThrow ConsoleM where
  throwM = liftIO . throwIO

instance MonadBackend ConsoleM where
  book item = runReaderT $ JIRA.bookM item
  search jql page = withClientEnv $ JIRA.searchM jql page

run :: [String] -> ConsoleM ()
run args = do
  config <- ask
  command <- runParser config args
  process command
  where
    process Version = printVersion

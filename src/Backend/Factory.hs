module Backend.Factory where

import           Backend
import qualified Backend.Impl.Archive as Archive
import qualified Backend.Impl.JIRA    as JIRA
import           BasicPrelude
import           Data.Aeson
import           Data.Foldable
import           Shared.Utils


nameToHandle :: MonadIO m => Value -> Text -> m (Maybe Backend.Handle)
nameToHandle config name
  | name == "jira" = case fromConfig config "jira" of
      Nothing -> pure Nothing
      Just jiraConfig -> Just <$> liftIO (JIRA.newHandle jiraConfig)
  | name == "archive" = case fromConfig config "archive" of
      Nothing -> pure Nothing
      Just archiveConfig -> Just <$> liftIO (Archive.newHandle archiveConfig)
  | otherwise = pure Nothing

withHandle :: Value -> (Backend.Handle -> IO a) -> IO a
withHandle config cont =
  let impls = ["jira", "archive"]
  in do
    handle <- fold . catMaybes <$> mapM (nameToHandle config) impls
    cont handle



{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Config (Config(..),
               loadConfig) where

import           Control.Monad.Except
import           Data.Aeson           (withObject, (.:))
import qualified Data.ByteString      as B
import qualified Data.Map.Strict      as M
import           Data.Text            hiding (index, words)
import qualified Data.Yaml            as YAML
import           Options.Applicative  hiding (execParser, runParser)
import           Prelude              hiding (readFile)

import           Backend
import           Tracker.Types


data Config = Config
  { baseURL        :: Text
  , password       :: Text
  , stateFile      :: FilePath
  , user           :: Text
  , defaultProject :: Text
  , issues         :: M.Map Text IssueKey
  , queries        :: M.Map Text JQL
  , aliases        :: M.Map String [String]
  }

-- invert :: (Ord k, Ord v) => M.Map k [v] -> M.Map v [k]
-- invert m = M.fromListWith (++) pairs
--     where pairs = [(v, [k]) | (k, vs) <- M.toList m, v <- vs]

-- reverseIssues :: Config -> M.Map IssueKey [Text]
-- reverseIssues = invert . fmap return . issues

instance YAML.FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .: "baseURL"
    <*> v .: "password"
    <*> v .: "stateFile"
    <*> v .: "user"
    <*> v .: "defaultProject"
    <*> v .: "issues"
    <*> (JQL <$>) `fmap` (v .: "jql")
    <*> (words <$>) `fmap` (v .: "aliases")

decodeEither :: MonadError Failure m => B.ByteString -> m Config
decodeEither s =
  case YAML.decodeEither s of
    Left err -> throwError err
    Right cfg -> return cfg

loadConfig :: (MonadError Failure m, MonadIO m) => FilePath -> m Config
loadConfig path = decodeEither =<< liftIO (B.readFile path)

-- makeEnv :: Config -> J.Env
-- makeEnv Config{..} = J.Env
--   { user = toS user
--   , password = toS password
--   , baseURL = toS baseURL }


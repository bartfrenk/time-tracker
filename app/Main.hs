{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BasicPrelude
import           Control.Lens          ((^?))
import           Control.Monad.Catch
import           Control.Monad.Managed
import           Control.Monad.Trans   (MonadIO)
import           Data.Aeson.Lens       (key)
import qualified Data.ByteString       as BS
import           Data.String.Conv      (toS)
import           Data.Yaml             (FromJSON, Value)

import qualified Backend.Impl.JIRA     as JIRA
import qualified Console
import           Shared.Utils
import qualified Tracker

loadConfig :: (MonadIO m, MonadThrow m) => FilePath -> m Value
loadConfig path = liftIO (BS.readFile path) >>= decodeThrowYAML

fromConfig :: (FromJSON a, MonadThrow m) => Value -> Text -> m a
fromConfig config sub = case config ^? key sub of
  Nothing    -> throwM $ ParseError $ "key \"" ++ toS sub ++ "\" not present"
  Just value -> fromValueThrow value

main :: IO ()
main = do
  config <- loadConfig "config.yaml"
  backendC <- fromConfig config "backend"
  trackerC <- fromConfig config "tracker"
  consoleC <- fromConfig config "console"

  runManaged $ do
    backendH <- managed $ JIRA.withHandle backendC
    trackerH <- managed $ Tracker.withHandle trackerC backendH
    consoleH <- managed $ Console.withHandle consoleC trackerH
    liftIO (Console.run consoleH =<< getArgs)
  return ()

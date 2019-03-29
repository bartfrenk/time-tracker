{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BasicPrelude
import           Control.Monad.Catch
import           Control.Monad.Managed
import           Control.Monad.Trans   (MonadIO)
import qualified Data.ByteString       as BS
import           Data.Yaml             (Value)
import           System.Directory      (getHomeDirectory)
import           System.Environment    (lookupEnv)

import qualified Backend.Factory  as Backend
import qualified Console
import           Shared.Utils
import qualified Tracker

loadConfig :: (MonadIO m, MonadThrow m) => FilePath -> m Value
loadConfig path = liftIO (BS.readFile path) >>= decodeThrowYAML

configPath :: IO FilePath
configPath = expandFilePath =<< do
  fromEnv <- lookupEnv "TRACK_CONFIG"
  home <- getHomeDirectory
  return $ fromMaybe (home </> ".track.yaml") fromEnv

main :: IO ()
main = do
  config <- loadConfig =<< configPath
  backendC <- fromConfig config "backend"
  trackerC <- fromConfig config "tracker"
  consoleC <- fromConfig config "console"

  void $ runManaged $ do
    backendH <- managed $ Backend.withHandle backendC
    trackerH <- managed $ Tracker.withHandle trackerC backendH
    consoleH <- managed $ Console.withHandle consoleC trackerH
    liftIO (Console.run consoleH =<< getArgs)

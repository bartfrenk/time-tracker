{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BasicPrelude
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Managed
import           Control.Monad.Trans   (MonadIO)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString       as BS
import           Data.String.Conv
import           Data.Yaml             as YAML

import qualified Backend.Impl.JIRA     as JIRA
import qualified Console
import qualified Tracker

decodeThrow :: MonadThrow m => ByteString -> m YAML.Value
decodeThrow bytes = case decodeEither bytes of
  Left _      -> undefined
  Right value -> return value

data ParseError = ParseError String deriving (Show, Typeable)

instance Exception ParseError

fromValueThrow :: (FromJSON a, MonadThrow m) => Value -> m a
fromValueThrow value = case fromJSON value of
  Error err -> throwM $ ParseError err
  Success a -> return a

loadConfig :: (MonadIO m, MonadThrow m) => FilePath -> m YAML.Value
loadConfig path = liftIO (BS.readFile path) >>= decodeThrow

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

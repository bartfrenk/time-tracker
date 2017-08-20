{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Console.Args where

import           BasicPrelude
import           Control.Monad.Trans (MonadIO)
import qualified Data.Text           as T
import           Options.Applicative

import           Console.Config      as Console
import           Shared.Types
import           Tracker.Types

data Command
  = Search Text
  | Start PartialIssueKey Timestamp
  | Stop Timestamp
  | Review
  | Book
  | Version
  deriving (Show)

runParser :: MonadIO m => Console.Config -> [Text] -> m Command
runParser config args = do
  now <- getTimestamp
  liftIO $ handleParseResult $ execParserPure defaultPrefs (p now) (T.unpack <$> args)
  where p ts = (helper <*> parseVersionFlag <|> parseCommand config ts) `info`
               (fullDesc <> progDesc "Time Tracker command line tool")

parseVersionFlag :: Parser Command
parseVersionFlag = flag' Version (long "version" <> help "Show version")

withInfo :: Parser a -> String -> ParserInfo a
withInfo p desc = p `info` progDesc desc

parseCommand :: Console.Config -> Timestamp -> Parser Command
parseCommand config now = hsubparser $
  command "start" (parseStart config now `withInfo` "Start work on an issue") <>
  command "stop" (parseStop config now `withInfo` "Stop work on active issue") <>
  command "review" (parseReview `withInfo` "Review logged work") <>
  command "book" (parseBook `withInfo` "Book logged work") <>
  command "search" (parseSearch `withInfo` "Search issues")

parseStart :: Console.Config -> Timestamp -> Parser Command
parseStart _ now = Start
  <$> argument auto (metavar "ISSUE")
  <*> timestampOption now

-- |Optional time offset argument applied to `now`, defaults to 0.
timestampOption :: Timestamp -> Parser Timestamp
timestampOption now = addTimeDelta now <$>
  option auto (short 't' <> metavar "TIME" <> value mempty)

parseStop :: Console.Config -> Timestamp -> Parser Command
parseStop _ now = Stop
  <$> timestampOption now

parseBook :: Parser Command
parseBook = pure Book

parseReview :: Parser Command
parseReview = pure Review

parseSearch :: Parser Command
parseSearch = Search
  <$> argument (T.pack <$> str) (metavar "QUERY")

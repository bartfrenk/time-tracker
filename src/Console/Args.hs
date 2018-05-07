{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Console.Args where

import           BasicPrelude hiding (first)
import           Control.Monad.Trans (MonadIO)
import           Data.Bifunctor (first)
import qualified Data.Text           as T
import           Options.Applicative

import           Console.Config      as Console
import           Shared.Types        hiding (Parser)
import           Tracker.Types

data StatusType
  = FullStatus
  | ActiveIssue
  deriving (Show)

data Command
  = Search Text
  | Start PartialIssueKey Timestamp
  | Stop Timestamp
  | Status StatusType
  | Book
  | Version
  | Close
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
  command "status" (parseStatus `withInfo` "Display status of logged work") <>
  command "book" (parseBook `withInfo` "Book logged work") <>
  command "search" (parseSearch `withInfo` "Search issues") <>
  command "close" (parseClose `withInfo`
                   "Truncate logging on issues that run past the `stopAt` time")

parseStart :: Console.Config -> Timestamp -> Parser Command
parseStart _ now = Start
  <$> argument auto (metavar "ISSUE")
  <*> extendedTimestampOption now

-- |Option for timestamp; the timestamp string consists of an optional local
-- time, and an optional time difference string, e.g. '9:00 -1d' parses as
-- yesterday 9:00 AM in the current time zone.
extendedTimestampOption :: Timestamp -> Parser Timestamp
extendedTimestampOption now =
  option reader
     ( short 't'
    <> metavar "TIME"
    <> value now
    <> help "Optional custom timestamp. The value consists of the base time \
            \and an offset. Both are optional. For example, '9:00 -1d' parses \
            \as yesterday 9:00 AM in the current time zone, and \
            \'2017-09-16 10:00 +1d4h' parses as 2:00 AM on 2017-09-17."
     )
  where
    reader = eitherReader (first show . fromExtendedTimestampString now)

parseClose :: Parser Command
parseClose = pure Close

parseStop :: Console.Config -> Timestamp -> Parser Command
parseStop _ now = Stop
  <$> extendedTimestampOption now

parseBook :: Parser Command
parseBook = pure Book

parseStatus :: Parser Command
parseStatus = Status <$>
  flag FullStatus ActiveIssue
     (long "active" <> help "Display only the active issue")

parseSearch :: Parser Command
parseSearch = Search
  <$> argument (T.pack <$> str) (metavar "QUERY")

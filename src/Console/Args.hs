{-# LANGUAGE NoImplicitPrelude #-}
module Console.Args where

import           BasicPrelude
import           Control.Monad.Trans (MonadIO)
import qualified Data.Text           as T
import           Options.Applicative

import           Console.Config      as Console
import           Shared.Types        (getTimestamp)
import           Tracker.Types

data Command
  = Search JQL
  | Start IssueKey Timestamp
  | Stop Timestamp
  | Review
  | Book
  | Version
  deriving (Eq, Show)

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
  command "start" (parseStart config now `withInfo` "Start work on an issue")

parseStart :: Console.Config -> Timestamp -> Parser Command
parseStart config now = Start
  <$> argument auto (metavar "ISSUE")
  <*> pure now






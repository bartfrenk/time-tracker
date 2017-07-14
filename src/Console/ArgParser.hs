{-# LANGUAGE OverloadedStrings #-}

module Console.ArgParser where

import           Control.Applicative (optional)
import           Control.Monad       (mzero)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Bifunctor      (first)
import qualified Data.Map.Strict     as M
import           Data.Semigroup      ((<>))
import           Data.String.Conv    (toS)
import           Data.Text           hiding (index, words)
import           Data.Time.LocalTime
import           Options.Applicative

import           Console.Config
import           Shared.Types
import           Tracker.State
import           Tracker.Types

data Command
  = Search JQL
  | Log IssueKey TimeDelta
  | Start IssueKey (Maybe Timestamp)
  | Stop (Maybe Timestamp)
  | Review
  | Book
  | Version
  | ListConfig deriving (Eq, Show)

-- |If a number, read number and prepend prefix, otherwise read full issue key.
readIssueKey :: Text -> ReadM IssueKey
readIssueKey prefix = auto
  -- fromText <$> txtParser
  -- where index = toS . show <$> (auto :: ReadM Int)
  --       txtParser = (prefix <>) <$> index <|> toS <$> str

-- |Replace parsed issue alias with a key in `issueAliases`.
expandIssue :: M.Map Text IssueKey -> ReadM IssueKey
expandIssue issueAliases = do
  parsed <- str
  case M.lookup (toS parsed) issueAliases of
    Nothing  -> mzero
    Just key -> return key

parseIssueKey :: Config -> ReadM IssueKey
parseIssueKey cfg =
  expandIssue (issues cfg) <|> readIssueKey (defaultPrefix cfg)

parseSearch :: Parser Command
parseSearch = Search <$> argument jql (metavar "JQL-QUERY")
  where jql = JQL . toS <$> str

parseStart :: Config -> ZonedTime -> Parser Command
parseStart opts zt = Start
  <$> argument (parseIssueKey opts) (metavar "ISSUE-KEY")
  <*> timeParser zt

timeParser :: ZonedTime -> Parser (Maybe Timestamp)
timeParser zt = undefined
  -- optional (option timeOffsetReader $ long "offset" <> metavar "TIME-OFFSET")
  -- where timeOffsetReader = eitherReader $ \s ->
  --         first show $ fromTimeOffsetString (toS s)

parseReview :: Parser Command
parseReview = pure Review

parseStop :: ZonedTime -> Parser Command
parseStop zt = Stop <$> timeParser zt

parseBook :: Parser Command
parseBook = pure Book

parseListConfig :: Parser Command
parseListConfig = pure ListConfig

withInfo :: Parser a -> String -> ParserInfo a
withInfo p desc = p `info` progDesc desc

defaultPrefix :: Config -> Text
defaultPrefix opts = defaultProject opts <> "-"

parseCommand :: Config -> ZonedTime -> Parser Command
parseCommand opts zt = hsubparser $
  command "search" (parseSearch `withInfo` "Search issues") <>
  command "start"  (parseStart opts zt `withInfo` "Start work on an issue") <>
  command "stop"   (parseStop zt `withInfo` "Stop work on active issue") <>
  command "review" (parseReview `withInfo` "Review logged work") <>
  command "book"   (parseBook `withInfo` "Book local worklog on JIRA") <>
  command "cfg"    (parseListConfig `withInfo` "List configuration")

parseVersionFlag :: Parser Command
parseVersionFlag = flag' Version (long "version" <> help "Show version")

runParser :: MonadIO m => Config -> [String] -> m Command
runParser opts args = do
  zt <- liftIO getZonedTime
  liftIO $ handleParseResult $ execParserPure defaultPrefs (p zt) args
  where p now = (helper <*> parseVersionFlag <|> parseCommand opts now) `info`
                (fullDesc <> progDesc "JIRA command line client")

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Tracker.Types
  ( IssueKey
  , toText
  , Issue(..)
  , LogItem(..)
  , Timestamp(..)
  , formatTimestamp
  , JQL(..)
  , Failure
  , toSeconds
  , mkIssueKey
  , completeToIssueKey
  ) where

import           BasicPrelude hiding ((<|>))
import           Data.Aeson
import           Data.Char    (isAlpha)
import qualified Data.Text    as T
import           GHC.Generics
import           Text.Parsec

import           Shared.Types

type Failure = String

data TrackerException
  = IssueNotFound IssueKey
  deriving (Show, Typeable)

instance Exception TrackerException

newtype IssueKey = IssueKey { toText :: T.Text } deriving (Eq)

newtype JQL = JQL T.Text deriving (Eq, Show)

newtype PartialIssueKey = PartialIssueKey Text

completeToIssueKey :: Text -> PartialIssueKey -> Either ParseError IssueKey
completeToIssueKey defaultProject (PartialIssueKey partial) =
  parse (issueKeyParser defaultProject) "" (T.unpack partial)

issueKeyParser :: Text -> Parsec String u IssueKey
issueKeyParser defaultPrefix = do
  project <-  many1 (satisfy isAlpha) <* char '-' <|> return (T.unpack defaultPrefix)
  index <- many1 digit
  return $ mkIssueKey $ T.pack (project ++ "-" ++ index)

instance Show IssueKey where
  show (IssueKey txt) = T.unpack txt

instance Read IssueKey where
  readsPrec _ s =
    let (word, rest) = span (/= ' ') (dropWhile (== ' ') s)
    in [(mkIssueKey $ T.pack word, rest)]

instance FromJSON IssueKey where
  parseJSON = (mkIssueKey <$>) . parseJSON

instance ToJSON IssueKey where
  toJSON (IssueKey txt) = toJSON txt

-- |Smart constructor for an issue key.
mkIssueKey :: T.Text -> IssueKey
mkIssueKey txt = IssueKey $ T.toUpper txt

data Issue = Issue
  { issueKey    :: IssueKey
  , summary     :: T.Text
  , storyPoints :: Maybe Double
  } deriving (Eq, Generic, Show)

instance FromJSON Issue
instance ToJSON Issue

data LogItem = LogItem
  { issueKey  :: IssueKey
  , started   :: Timestamp
  , timeSpent :: TimeDelta
  } deriving (Eq, Show)

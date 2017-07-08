{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Tracker.Types (IssueKey,
                       Issue(..),
                       LogItem(..),
                       Timestamp(..),
                       formatTimestamp,
                       JQL(..),
                       Failure,
                       toSeconds,
                       mkIssueKey) where

import           Data.Aeson
import qualified Data.Text           as T
import           GHC.Generics

import           Shared.Types

type Failure = String

newtype IssueKey = IssueKey T.Text deriving (Eq)

newtype JQL = JQL T.Text deriving (Eq, Show)

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

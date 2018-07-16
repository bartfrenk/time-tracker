{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Tracker.Types
  ( IssueKey
  , PartialIssueKey
  , TrackerException(..)
  , toText
  , Issue(..)
  , LogItem(..)
  , Timestamp(..)
  , formatTimestamp
  , JQL(..)
  , toSeconds
  , mkIssueKey
  , completeToIssueKey
  , Config(..)
  , Event(..)
  ) where

import           BasicPrelude         hiding (lookup, (<|>))
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Char            (isAlpha)
import           Data.Map.Strict      (lookup)
import qualified Data.Text            as T
import           GHC.Generics
import           Text.Parsec

import           Shared.Types

data TrackerException
  = IssueNotFound IssueKey
  | InvalidIssueKey Text
  deriving (Show, Typeable)

instance Exception TrackerException

data Event
  = Started Timestamp
            IssueKey
  | Stopped Timestamp
  deriving (Eq, Show, Read)

newtype IssueKey = IssueKey
  { toText :: T.Text
  } deriving (Eq)

newtype JQL =
  JQL T.Text
  deriving (Eq, Show)

instance FromJSON JQL where
  parseJSON = (JQL <$>) . parseJSON

instance Read JQL where
  readsPrec _ s = [(JQL (T.pack s), "")]

newtype PartialIssueKey =
  PartialIssueKey Text

instance Read PartialIssueKey where
  readsPrec _ s =
    let (word, rest) = span (/= ' ') (dropWhile (== ' ') s)
    in [(PartialIssueKey $ T.pack word, rest)]

instance Show PartialIssueKey where
  show (PartialIssueKey t) = T.unpack t

expandIssueAliases :: MonadReader Config m => Text -> m (Maybe IssueKey)
expandIssueAliases t = lookup t <$> reader issues

-- | Completes the partial issue key to a project-qualified issue key.
completeToIssueKey ::
     (MonadThrow m, MonadReader Config m) => PartialIssueKey -> m IssueKey
completeToIssueKey (PartialIssueKey t) = do
  issueKey' <- expandIssueAliases t
  case issueKey' of
    Nothing -> parseIssueKey t
    Just key -> return key

parseIssueKey :: (MonadThrow m, MonadReader Config m) => Text -> m IssueKey
parseIssueKey t = do
  project <- reader defaultProject
  case parse (issueKeyParser project) "" (T.unpack t) of
    Left _ -> throwM $ InvalidIssueKey t
    Right res -> return res

issueKeyParser :: Text -> Parsec String u IssueKey
issueKeyParser defaultPrefix = do
  project <-
    many1 (satisfy isAlpha) <* char '-' <|> return (T.unpack defaultPrefix)
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

data Config = Config
  { statePath      :: FilePath
  , defaultProject :: Text
  , stopAt         :: TimeOfDay
  , issues         :: Map Text IssueKey
  , queries        :: Map Text JQL
  } deriving (Show, Generic)

instance FromJSON Config

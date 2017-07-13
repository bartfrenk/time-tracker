{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}

module Backend.Impl.JIRA
  ( module Backend.Impl.JIRA
  , ClientEnv
  , runClientT
  ) where

import           BasicPrelude
import           Control.Lens
import           Control.Monad.Catch
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types      (Parser, parseEither)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy  as Lazy
import           Data.String.Conv
import           Network.HTTP.Client

import           Backend
import           Shared.Client
import           Tracker.Types

searchM :: MonadClient m => JQL -> Page -> m [Issue]
searchM jql page =
  mkSearchRequest jql page >>= http >>= parseSearchResponse

mkSearchRequest :: (MonadReader ClientEnv m, MonadThrow m)
                => JQL -> Page -> m Request
mkSearchRequest jql page = do
  request <- mkRequestThrow GET "/rest/api/2/search"
  let query = setPage page
            $ setFields ["key", "summary" , "issuetype", "customfield_10002"]
            $ setJQL jql []
  return $ request { queryString = encodeQuery query }
    where setPage Page{..} query =
            ("startAt", Just $ toS (tshow offset)):
            ("maxResults", Just $ toS (tshow limit)):query
          setFields fields query =
            ("fields", Just $ C8.intercalate "," fields):query
          setJQL (JQL t) query = ("jql", Just $ toS t):query

parseSearchResponse :: MonadThrow m
                    => Response Lazy.ByteString -> m [Issue]
parseSearchResponse response =
  jsonToIssue `mapM` (responseBody response ^.. key "issues" . values)
  where jsonToIssue = parseThrow issueParser
        issueParser = withObject "IssueWrap" $ \v -> Issue
          <$> (mkIssueKey <$> v .: "key")
          <*> ((v .: "fields") >>= (.: "summary"))
          <*> ((v .: "fields") >>= (.: "customfield_10002"))

bookM :: MonadClient m => LogItem -> m ()
bookM = undefined

parseThrow :: MonadThrow m => (a -> Parser b) -> a -> m b
parseThrow p v = case parseEither p v of
  Left _  -> undefined
  Right b -> return b

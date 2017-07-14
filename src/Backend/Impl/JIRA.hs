{-# LANGUAGE FlexibleInstances     #-}
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
  , Config(..))
  where

import           BasicPrelude              hiding (try)
import           Control.Lens
import           Control.Monad             (unless, (>=>))
import           Control.Monad.Catch
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types          (Parser, parseEither)
import qualified Data.ByteString.Char8     as C8
import qualified Data.ByteString.Lazy      as Lazy
import           Data.String.Conv
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (notFound404, ok200)

import           Backend
import           Shared.Client             as Client
import           Tracker.Types

withHandle :: Client.Config -> (Backend.Handle -> IO a) -> IO a
withHandle config cont = liftIO (newHandle config) >>= cont

newHandle :: Client.Config -> IO Backend.Handle
newHandle config = do
  env <- mkDefaultClientEnv config
  return Backend.Handle
    { book = \item -> runClientT (bookM item) env
    , search = \jql page -> runClientT (searchM jql page) env
    , fetch = \key -> runClientT (fetchM key) env
    }

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
          setJQL (JQL t) query = ("jql", Just $ toS t):query

setFields :: [ByteString] -> Query -> Query
setFields fields query = ("fields", Just $ C8.intercalate "," fields):query

parseSearchResponse :: MonadThrow m
                    => Response Lazy.ByteString -> m [Issue]
parseSearchResponse response =
  jsonToIssue `mapM` (responseBody response ^.. key "issues" . values)

jsonToIssue :: MonadThrow m => Value -> m Issue
jsonToIssue = parseThrow issueParser
  where issueParser = withObject "IssueWrap" $ \v -> Issue
          <$> (mkIssueKey <$> v .: "key")
          <*> ((v .: "fields") >>= (.: "summary"))
          <*> ((v .: "fields") >>= (.: "customfield_10002"))

fetchM :: MonadClient m => IssueKey -> m (Maybe Issue)
fetchM key = do
  resp <- mkFetchRequest key >>= http
  if responseStatus resp == ok200
    then Just <$> (decodeThrow >=> jsonToIssue) (responseBody resp)
    else return Nothing

mkFetchRequest :: (MonadReader ClientEnv m, MonadThrow m)
               => IssueKey -> m Request
mkFetchRequest key = do
  request <- mkRequest GET ("/rest/api/2/issue/" ++ show key)
  let query = setFields ["key", "summary" , "issuetype", "customfield_10002"] []
  return request
    { checkResponse = \req resp ->
        unless (responseStatus resp `elem` [notFound404, ok200])
               (checkResponse request req resp)
    , queryString = encodeQuery query
    }

bookM :: MonadClient m => LogItem -> m ()
bookM = undefined

parseThrow :: MonadThrow m => (a -> Parser b) -> a -> m b
parseThrow p v = case parseEither p v of
  Left _  -> undefined -- TODO
  Right b -> return b

decodeThrow :: (MonadThrow m, FromJSON a) => Lazy.ByteString -> m a
decodeThrow bs = case eitherDecode bs of
  Left _ -> undefined -- TODO
  Right a -> return a



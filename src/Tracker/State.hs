{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Tracker.State where

import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.State hiding (state)
import           Control.Monad.Trans (MonadIO)
import           Data.Bifunctor      (first)
import           Data.Typeable
import           Safe
import           System.Directory    (doesFileExist)
import           Text.Read

import           Shared.Types
import           Tracker.Types

data StateException
  = ParseException String
  | InvalidState String
  deriving (Show, Typeable)

instance Exception StateException

data Event
  = Started Timestamp IssueKey
  | Stopped Timestamp deriving (Eq, Show, Read)

newtype LocalState = LocalState [Event] deriving (Eq, Show)

eventTimestamp :: Event -> Timestamp
eventTimestamp (Started ts _) = ts
eventTimestamp (Stopped ts)   = ts

lastEvent :: LocalState -> Maybe Event
lastEvent (LocalState events) = lastMay events

-- |Loads state from file at location `path`. Returns empty local state if file
-- does not exist. Might fail due to parse errors.
loadState :: (MonadIO m, MonadThrow m)
          => FilePath -> m LocalState
loadState path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then do raw <- liftIO $ readFile path
            case readMaybe `traverse` filter (not . blank) (lines raw) of
              Nothing     -> throwM $ ParseException $ "Unable to parse " ++ path
              Just events -> return $ LocalState events
    else return $ LocalState []

blank :: String -> Bool
blank = all (== ' ')

-- |Saves the state to file at location `path`.
saveState :: MonadIO m => FilePath -> LocalState -> m ()
saveState path (LocalState events) =
  liftIO $ writeFile path $ unlines (show `fmap` events)

-- |Returns the key of the active issue, if there is any.
active :: LocalState -> Maybe (Timestamp, IssueKey)
active (LocalState events) = case last events of
  Started ts key -> Just (ts, key)
  _              -> Nothing

-- |Takes the first log item from the state, and returns it, together with the
-- remaining events in the state.
takeLogItem :: LocalState -> (LocalState, Maybe LogItem)
takeLogItem (LocalState events) =
  LocalState `first` takeLogItem' events
  where
    takeLogItem' :: [Event] -> ([Event], Maybe LogItem)
    takeLogItem' (Started start key:Stopped end:rest) =
      (rest, Just $ LogItem key start $ duration start end)
    takeLogItem' (Started start key:second@(Started end _):rest) =
      (second:rest, Just $ LogItem key start $ duration start end)
    takeLogItem' singleton@[Started _ _] = (singleton, Nothing)
    takeLogItem' (Stopped _:rest) = takeLogItem' rest
    takeLogItem' [] = ([], Nothing)

takeAllLogItems :: LocalState -> (LocalState, [LogItem])
takeAllLogItems state =
  let (state', item') = takeLogItem state
  in case item' of
    Nothing -> (state', [])
    Just item ->
      let (state'', items) = takeAllLogItems state'
      in (state'', item:items)

-- |Appends an event to the end of the event log in the state.
appendEvent :: (MonadState LocalState m, MonadThrow m)
               => Event -> m ()
appendEvent event = do
  gets lastEvent >>= flip checkEvent event
  modify $ append event
  where
    append new (LocalState events) = LocalState $ events ++ [new]

    checkEvent Nothing (Stopped _)
      = throwM $ InvalidState "No active task"
    checkEvent Nothing (Started _ _)
      = return ()
    checkEvent (Just (Stopped _)) (Stopped _)
      = throwM $ InvalidState "No active task"
    checkEvent (Just (Stopped ts1)) (Started ts2 _)
      | ts1 <= ts2 = return ()
      | otherwise = throwM $ InvalidState "Timestamps need to be non-decreasing"
    checkEvent (Just (Started ts1 _)) (Stopped ts2)
      | ts1 <= ts2 = return ()
      | otherwise = throwM $ InvalidState "Timestamps need to be non-decreasing"
    checkEvent (Just (Started ts1 key1)) (Started ts2 key2)
      | ts1 > ts2 = throwM $ InvalidState "Timestamps need to be non-decreasing"
      | key1 == key2 = throwM $ InvalidState $ "Issue " ++ show key1 ++ " is already active"
      | otherwise = return ()

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Tracker.State where

import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans  (MonadIO)
import           Data.Bifunctor       (first)
import           Data.Time.Clock      (diffUTCTime)
import           Data.Time.LocalTime
import           Safe
import           System.Directory     (doesFileExist)
import           Text.Read

import           Shared.Types
import           Tracker.Types

getCurrentTimestamp :: MonadIO m => m Timestamp
getCurrentTimestamp = Timestamp <$> liftIO getZonedTime

lastEvent :: LocalState -> Maybe Event
lastEvent (LocalState events) = lastMay events

duration :: Timestamp -> Timestamp -> TimeDelta
duration (Timestamp start) (Timestamp end) =
  let delta = diffUTCTime (zonedTimeToUTC end) (zonedTimeToUTC start)
  in TimeDelta $ truncate (realToFrac delta :: Double)

data Event
  = Started Timestamp IssueKey
  | Stopped Timestamp deriving (Eq, Show, Read)

newtype LocalState = LocalState [Event] deriving (Show)

eventTimestamp :: Event -> Timestamp
eventTimestamp (Started ts _) = ts
eventTimestamp (Stopped ts)   = ts

-- |Loads state from file at location `path`. Returns empty local state if file
-- does not exist. Might fail due to parse errors.
loadState :: FilePath -> ExceptT Failure IO LocalState
loadState path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then do raw <- liftIO $ readFile path
            case readMaybe `traverse` filter (not . blank) (lines raw) of
              Nothing     -> throwError $ "Unable to parse " ++ path
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
tryAppendEvent :: (MonadState LocalState m, MonadError Failure m) => Event -> m ()
tryAppendEvent event = do
  last <- gets lastEvent
  event' <- checkEvent last event
  modify $ append event
  --LocalState (events ++ [event])
  where
    append event (LocalState events) = LocalState $ events ++ [event]

    checkEvent Nothing (Stopped _)
      = throwError "No active task"
    checkEvent Nothing new@(Started _ _)
      = return new
    checkEvent (Just (Stopped _)) (Stopped _)
      = throwError "No active task"
    checkEvent (Just (Stopped ts1)) new@(Started ts2 _)
      | ts1 <= ts2 = return new
      | otherwise = throwError "Timestamps need to be non-decreasing"
    checkEvent (Just (Started ts1 _)) new@(Stopped ts2)
      | ts1 <= ts2 = return new
      | otherwise = throwError "Timestamps need to be non-decreasing"
    checkEvent (Just (Started ts1 _)) new@(Started ts2 _)
      | ts1 <= ts2 = return new
      | otherwise = throwError "Timestamps need to be non-decreasing"

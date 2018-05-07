{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Tracker.State where

import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.State hiding (state)
import           Control.Monad.Trans (MonadIO)
import           Data.Bifunctor      (first)
import           Data.Time.LocalTime hiding (TimeOfDay)
import           Data.Typeable
import           Safe
import           System.Directory    (doesFileExist)
import           Text.Read           hiding (get)

import Debug.Trace

import           Shared.Types
import           Tracker.Types

data StateException
  = ParseException String
  | InvalidState String
  deriving (Show, Typeable)

instance Exception StateException

newtype LocalState =
  LocalState [Event]
  deriving (Eq, Show)

eventTimestamp :: Event -> Timestamp
eventTimestamp (Started ts _) = ts
eventTimestamp (Stopped ts) = ts

lastEvent :: LocalState -> Maybe Event
lastEvent (LocalState events) = lastMay events

-- |Loads state from file at location `path`. Returns empty local state if file
-- does not exist. Might fail due to parse errors.
loadState :: (MonadIO m, MonadThrow m) => FilePath -> m LocalState
loadState path = do
  exists <- liftIO $ doesFileExist path
  if exists
    then do
      raw <- liftIO $ readFile path
      case readMaybe `traverse` filter (not . blank) (lines raw) of
        Nothing -> throwM $ ParseException $ "Unable to parse " ++ path
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
active (LocalState events) =
  case last events of
    Started ts key -> Just (ts, key)
    _ -> Nothing

-- |Takes the first log item from the state, and returns it, together with the
-- remaining events in the state.
takeLogItem :: LocalState -> (LocalState, Maybe LogItem)
takeLogItem (LocalState events) = LocalState `first` takeLogItem' events
  where
    takeLogItem' :: [Event] -> ([Event], Maybe LogItem)
    takeLogItem' (Started start key:Stopped end:rest) =
      (rest, Just $ LogItem key start $ duration start end)
    takeLogItem' (Started start key:second@(Started end _):rest) =
      (second : rest, Just $ LogItem key start $ duration start end)
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
         in (state'', item : items)

readActiveLogItem :: MonadState LocalState m => Timestamp -> m (Maybe LogItem)
readActiveLogItem ts = gets lastEvent >>= mkLogItemUntil ts
  where
    mkLogItemUntil ts2 event' =
      case event' of
        Just (Started ts1 key) ->
          return $ Just $ LogItem key ts1 (duration ts1 ts2)
        _ -> return Nothing

readLastLogItem :: MonadState LocalState m => m (Maybe LogItem)
readLastLogItem = gets (lastMay . snd . takeAllLogItems)

-- |Appends an event to the end of the event log in the state.
appendEvent :: (MonadState LocalState m, MonadThrow m) => Event -> m ()
appendEvent event = do
  gets lastEvent >>= flip checkEvent event
  modify $ append event
  where
    append new (LocalState events) = LocalState $ events ++ [new]
    checkEvent Nothing (Stopped _) = throwM $ InvalidState "No active task"
    checkEvent Nothing (Started _ _) = return ()
    checkEvent (Just (Stopped _)) (Stopped _) =
      throwM $ InvalidState "No active task"
    checkEvent (Just (Stopped ts1)) (Started ts2 _)
      | ts1 <= ts2 = return ()
      | otherwise = throwM $ InvalidState "Timestamps need to be non-decreasing"
    checkEvent (Just (Started ts1 _)) (Stopped ts2)
      | ts1 <= ts2 = return ()
      | otherwise = throwM $ InvalidState "Timestamps need to be non-decreasing"
    checkEvent (Just (Started ts1 key1)) (Started ts2 key2)
      | ts1 > ts2 = throwM $ InvalidState "Timestamps need to be non-decreasing"
      | key1 == key2 =
        throwM $ InvalidState $ "Issue " ++ show key1 ++ " is already active"
      | otherwise = return ()

closeForgotten :: MonadState LocalState m => Timestamp -> TimeOfDay -> m [Event]
closeForgotten now t = do
  LocalState events <- get
  let (newState, inserted) = insertStops events
  put $ LocalState newState
  pure inserted
  where
    insertStops :: [Event] -> ([Event], [Event])
    insertStops (e1:(e2:rest)) =
      let es = insertBetween e1 e2
          (events, inserted) = insertStops (e2:rest)
      in ([e1] ++ es ++ events, es ++ inserted)
    insertStops es@[(Started start _)] =
      case timeWithin t start now of
        Nothing -> (es, [])
        Just new -> (es ++ [Stopped new], [Stopped new])
    insertStops es  = (es, [])

    insertBetween :: Event -> Event -> [Event]
    insertBetween (Started ts1 _) (Started ts2 _) =
      case timeWithin t ts1 ts2 of
        Nothing -> []
        Just ts -> [Stopped ts]
    insertBetween _ _ = []

timeWithin :: TimeOfDay -> Timestamp -> Timestamp -> Maybe Timestamp
timeWithin (TimeOfDay timeOfDay) start@(Timestamp startTime) end@(Timestamp endTime) =
  let tz = zonedTimeZone startTime
      startDay = localDay $ zonedTimeToLocalTime startTime
      timestamp = Timestamp $ ZonedTime (LocalTime startDay timeOfDay) tz
      endDay = localDay $ zonedTimeToLocalTime endTime
  in if start < timestamp && timestamp < end && startDay < endDay
       then Just timestamp
       else Nothing

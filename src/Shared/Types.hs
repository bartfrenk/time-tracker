{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Shared.Types where

import           BasicPrelude
import           Data.Time.Calendar
import           Data.Time.Clock     (diffUTCTime)
import           Data.Time.Format
import           Data.Time.LocalTime

data Result a
  = Failure Text
  | Success a deriving (Show)

newtype TimeDelta = TimeDelta
  { toSeconds :: Integer
  } deriving (Eq, Show)

instance Monoid TimeDelta where
  mempty = TimeDelta 0
  (TimeDelta d1) `mappend` (TimeDelta d2) = TimeDelta (d1 + d2)

duration :: Timestamp -> Timestamp -> TimeDelta
duration (Timestamp start) (Timestamp end) =
  let delta = diffUTCTime (zonedTimeToUTC end) (zonedTimeToUTC start)
  in TimeDelta $ truncate (realToFrac delta :: Double)

toDurationString :: TimeDelta -> Text
toDurationString = concat . toDurationList

-- |Maps to a list of the form [2d, 3h, 2m, 1s], without zero entries.
toDurationList :: TimeDelta -> [Text]
toDurationList (TimeDelta s) | s >= 0 =
  let seconds = scanr (*) 1 [24, 60, 60]
      periods = ["d", "h", "m", "s"]
      perPeriod = breakdown s seconds
      nonZero = filter ((/= 0) . fst) $ zip perPeriod periods
  in (\(n, per) -> tshow n <> per) <$> nonZero
toDurationList (TimeDelta s)
  | s < 0 = "-" : toDurationList (TimeDelta (-s))
toDurationList (TimeDelta _) = undefined -- does not happen

breakdown :: Integer -> [Integer] -> [Integer]
breakdown n (x:xs) = (n `div` x):breakdown (n `mod` x) xs
breakdown _ []     = []

newtype Timestamp = Timestamp ZonedTime

instance Ord Timestamp where
  (Timestamp zt1) `compare` (Timestamp zt2) =
    zonedTimeToUTC zt1 `compare` zonedTimeToUTC zt2

instance Show Timestamp where
  show (Timestamp zt) =
    formatTime defaultTimeLocale timestampFormatStr zt

instance Read Timestamp where
  readsPrec _ = \s -> first Timestamp <$> p s
    where p = readSTime False defaultTimeLocale timestampFormatStr

instance Eq Timestamp where
  ts1 == ts2 = show ts1 == show ts2

getTimestamp :: MonadIO m => m Timestamp
getTimestamp = Timestamp <$> liftIO getZonedTime

timestampFormatStr :: String
timestampFormatStr = "%Y-%m-%d %H:%M %z"

formatTimestamp :: TimeLocale -> String -> Timestamp -> String
formatTimestamp locale formatStr (Timestamp zt) =
  formatTime locale formatStr zt

getDay :: Timestamp -> Day
getDay (Timestamp (ZonedTime local _)) = localDay local

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}

module Shared.Types where

import           BasicPrelude
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Time.Clock     (diffUTCTime)

data Result a
  = Failure Text
  | Success a deriving (Show)

newtype TimeDelta = TimeDelta
  { toSeconds :: Integer
  } deriving (Eq, Show)

duration :: Timestamp -> Timestamp -> TimeDelta
duration (Timestamp start) (Timestamp end) =
  let delta = diffUTCTime (zonedTimeToUTC end) (zonedTimeToUTC start)
  in TimeDelta $ truncate (realToFrac delta :: Double)

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

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Shared.Types where

import           BasicPrelude
import qualified Data.Text           as T
import           Data.Time.Calendar
import           Data.Time.Clock     (diffUTCTime, addUTCTime)
import           Data.Time.Format
import           Data.Time.LocalTime
import           Text.Parsec


data Result a
  = Failure T.Text
  | Success a deriving (Show)

newtype TimeDelta = TimeDelta
  { toSeconds :: Integer
  } deriving Eq

instance Monoid TimeDelta where
  mempty = TimeDelta 0
  (TimeDelta d1) `mappend` (TimeDelta d2) = TimeDelta (d1 + d2)

instance Show TimeDelta where
  show = T.unpack . toDurationString

instance Read TimeDelta where
  readsPrec = parsecToReadsPrec timeDeltaParser

-- |Combine parser to return remaining input
withRemaining :: Parsec String u a -> Parsec String u (a, String)
withRemaining p = (,) <$> p <*> getInput

-- |Convert Parsec parser to a Read instance parser
parsecToReadsPrec :: Parsec String () a -> Int -> ReadS a
parsecToReadsPrec parsecParser _ input
    = case parse (withRemaining parsecParser) "" input of
        Left _       -> []
        Right result -> [result]

-- |Parser to convert strings of the form '1h3d2m1s' to seconds.
timeDeltaParser :: Parsec String u TimeDelta
timeDeltaParser = TimeDelta <$> directed
  where directed = (*) <$> minus <*> (sum <$> sepBy1 seconds whitespace)
        number = read . T.pack <$> many1 digit
        seconds = countSeconds <$> number <*> oneOf ['d', 'h', 'm', 's']
        whitespace = many $ char ' '
        minus = option 1 (char '-' >> return (-1))
        countSeconds :: Integer -> Char -> Integer
        countSeconds n 'd' = n * 24 * 60 * 60
        countSeconds n 'h' = n * 60 * 60
        countSeconds n 'm' = n * 60
        countSeconds n 's' = n
        countSeconds _ _   = error ""

duration :: Timestamp -> Timestamp -> TimeDelta
duration (Timestamp start) (Timestamp end) =
  let delta = diffUTCTime (zonedTimeToUTC end) (zonedTimeToUTC start)
  in TimeDelta $ truncate (realToFrac delta :: Double)

addTimeDelta :: Timestamp -> TimeDelta -> Timestamp
addTimeDelta (Timestamp zt@(ZonedTime _ tz)) (TimeDelta sec) =
  let t = zonedTimeToUTC zt
      dt = fromInteger sec
  in Timestamp $ utcToZonedTime tz (addUTCTime dt t)

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

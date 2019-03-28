module Shared.Types where

-- TODO: specify public members
import Control.Arrow
import           BasicPrelude          hiding ((<|>))
import           Data.Aeson
import           Data.Functor.Identity
import qualified Data.Text             as T
import           Data.Time.Calendar
import qualified Data.Time.LocalTime as L
import           Data.Time.Clock       (addUTCTime, diffUTCTime)
import           Data.Time.Format
import           Data.Time.LocalTime hiding (TimeOfDay)
import           Text.Parsec
import           Text.Read             (readMaybe)

data Result a
  = Failure T.Text
  | Success a
  deriving (Show)

newtype TimeOfDay = TimeOfDay L.TimeOfDay

instance Read TimeOfDay where
  readsPrec _ = \s ->
    let parses = readSTime True defaultTimeLocale "%H:%M" s
    in first TimeOfDay <$> parses

instance Show TimeOfDay where
  show (TimeOfDay timeOfDay) =
    formatTime defaultTimeLocale "%H:%M" timeOfDay

instance FromJSON TimeOfDay where
  parseJSON =
    withText "LocalTimeOfDay" $ \s ->
      case readMaybe (T.unpack s) of
        Nothing -> fail "Failed to parse local time of day"
        Just t -> pure t

newtype TimeDelta = TimeDelta
  { toSeconds :: Integer
  } deriving (Eq)

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
parsecToReadsPrec parsecParser _ input =
  case parse (withRemaining parsecParser) "" input of
    Left _ -> []
    Right result -> [result]

-- |Parser to convert strings of the form '1h3d2m1s' to seconds.
timeDeltaParser :: Parsec String () TimeDelta
timeDeltaParser = TimeDelta <$> directed
  where
    directed = (*) <$> minus <*> (sum <$> sepBy1 seconds whitespace)
    number = read . T.pack <$> many1 digit
    seconds = countSeconds <$> number <*> oneOf ['d', 'h', 'm', 's']
    minus = option 1 ((char '-' >> return (-1)) <|> (char '+' >> return 1))
    countSeconds :: Integer -> Char -> Integer
    countSeconds n 'd' = n * 24 * 60 * 60
    countSeconds n 'h' = n * 60 * 60
    countSeconds n 'm' = n * 60
    countSeconds n 's' = n
    countSeconds _ _ = error ""

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
toDurationList (TimeDelta s)
  | s >= 0 =
    let seconds = scanr (*) 1 [24, 60, 60]
        periods = ["d", "h", "m", "s"]
        perPeriod = breakdown s seconds
        nonZero = filter ((/= 0) . fst) $ zip perPeriod periods
    in (\(n, per) -> tshow n <> per) <$> nonZero
toDurationList (TimeDelta s)
  | s < 0 = "-" : toDurationList (TimeDelta (-s))
toDurationList (TimeDelta _) = undefined -- does not happen

breakdown :: Integer -> [Integer] -> [Integer]
breakdown n (x:xs) = (n `div` x) : breakdown (n `mod` x) xs
breakdown _ [] = []

newtype Timestamp =
  Timestamp ZonedTime

instance Ord Timestamp where
  (Timestamp zt1) `compare` (Timestamp zt2) =
    zonedTimeToUTC zt1 `compare` zonedTimeToUTC zt2

instance Show Timestamp where
  show (Timestamp zt) = formatTime defaultTimeLocale timestampFormatStr zt

fromExtendedTimestampString ::
     Timestamp -> String -> Either ParseError Timestamp
fromExtendedTimestampString now = parse (extendedTimestampParser now) ""

extendedTimestampParser :: Timestamp -> Parsec String () Timestamp
extendedTimestampParser (Timestamp zt) = do
  ts <- localTimestampParser zt
  whitespace
  delta <- try timeDeltaParser <|> return mempty
  return $ addTimeDelta ts delta
  where
    localTimestampParser (ZonedTime local tz) =
      Timestamp <$> (ZonedTime <$> dateTimeParser local <*> pure tz)

smallNumber :: CharStream s => Parser s Int
smallNumber = read . T.pack <$> (try (count 2 digit) <|> count 1 digit)

type CharStream s = Stream s Identity Char

type Parser s = Parsec s ()

whitespace :: CharStream s => Parser s ()
whitespace = void $ many $ oneOf spaceChars

spaceChars :: String
spaceChars = " \n\t"

dateTimeParser :: CharStream s => LocalTime -> Parser s LocalTime
dateTimeParser (LocalTime day timeOfDay) = do
  day' <- try dayParser <|> return day
  whitespace
  timeOfDay' <- try timeParser <|> return timeOfDay
  return $ LocalTime day' timeOfDay'

dayParser :: CharStream s => Parser s Day
dayParser = do
  year <- read . T.pack <$> count 4 digit
  void (char '-')
  month <- smallNumber
  void (char '-')
  day <- smallNumber
  return $ fromGregorian year month day

timeParser :: CharStream s => Parser s L.TimeOfDay
timeParser = do
  hour <- smallNumber
  void (char ':')
  minute <- smallNumber
  return $ L.TimeOfDay hour minute 0

instance Read Timestamp where
  readsPrec _ = \s -> first Timestamp <$> p s
    where
      p = readSTime False defaultTimeLocale timestampFormatStr

instance Eq Timestamp where
  ts1 == ts2 = show ts1 == show ts2

getTimestamp :: MonadIO m => m Timestamp
getTimestamp = Timestamp <$> liftIO getZonedTime

timestampFormatStr :: String
timestampFormatStr = "%Y-%m-%d %H:%M %z"

formatTimestamp :: TimeLocale -> String -> Timestamp -> String
formatTimestamp locale formatStr (Timestamp zt) = formatTime locale formatStr zt

getDay :: Timestamp -> Day
getDay (Timestamp (ZonedTime local _)) = localDay local

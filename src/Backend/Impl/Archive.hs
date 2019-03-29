module Backend.Impl.Archive where

import           Backend          (Handle (..))
import           BasicPrelude
import           Data.Aeson
import qualified Data.Text        as T
import           Data.Time.Format (defaultTimeLocale)
import           GHC.Generics
import           Shared.Types     (addTimeDelta, timestampFormatStr)
import           Shared.Utils
import           Tracker.Types    hiding (Config)

data Config = Config
  { archivePath :: FilePath
  , timeFormat  :: Maybe String
  } deriving (Eq, Show, Generic)

instance FromJSON Config

newHandle :: Config -> IO Backend.Handle
newHandle config = pure $ mempty
  { book = writeLogItem config
  }

toCSV :: Config -> LogItem -> Text
toCSV Config {timeFormat} LogItem {issueKey, started, timeSpent} =
  toText issueKey <> ", " <> startedTxt <> ", " <> endTxt
  where
    fmt = fromMaybe timestampFormatStr timeFormat
    startedTxt = T.pack $
      formatTimestamp defaultTimeLocale fmt started
    end = addTimeDelta started timeSpent
    endTxt = T.pack $
      formatTimestamp defaultTimeLocale fmt end

writeLogItem :: MonadIO m => Config -> LogItem -> m ()
writeLogItem config@Config {archivePath} item = do
  expandedPath <- liftIO (expandFilePath archivePath)
  appendFile expandedPath (toCSV config item <> "\n")



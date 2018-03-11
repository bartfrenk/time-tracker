{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Console.Format where

import           BasicPrelude
import           Control.Monad.Trans          (MonadIO)
import qualified Data.Map.Strict              as M
import qualified Data.Text                    as T
import           Data.Time.Calendar
import           Data.Time.Format             (defaultTimeLocale)
import qualified Text.PrettyPrint.ANSI.Leijen as P


import           Shared.Types
import           Tracker.Types


(</$>) :: P.Doc -> P.Doc -> P.Doc
(</$>) = (P.<$>)

(</$$>) :: P.Doc -> P.Doc -> P.Doc
doc1 </$$> doc2 = doc1 </$> P.line <> doc2

txt :: Text -> P.Doc
txt = P.text . T.unpack

printIssue :: MonadIO m => Issue -> m ()
printIssue = liftIO . P.putDoc . formatIssue

formatIssue :: Issue -> P.Doc
formatIssue issue = txt
  $ toText (issueKey (issue :: Issue)) <> "\t"
  <> maybe "-" tshow (storyPoints issue) <> "\t"
  <> summary issue <> "\n"

printFullStatus :: MonadIO m => ([LogItem], Maybe LogItem) -> m ()
printFullStatus (history, active) =
  let historyText = logItemToText <$> history
      activeText = logItemToText <$> active
      formatFn = mkFormatFn "  " $ historyText <> maybeToList activeText
  in liftIO $ P.putDoc $ P.empty </$>
       P.indent 2 (
         formatHistory formatFn history </$$>
         formatActive formatFn active
       ) </$$> P.empty

printActiveIssue :: MonadIO m => ([LogItem], Maybe LogItem) -> m ()
printActiveIssue (_, active') =
  case active' of
    Nothing -> liftIO $ P.putDoc P.empty
    Just active -> do
      let [k, _, _] = logItemToText active
      liftIO $ P.putDoc $ (txt k) </$> P.empty

formatActive :: ([Text] -> P.Doc) -> Maybe LogItem -> P.Doc
formatActive _ Nothing = "No active issue"
formatActive formatFn (Just active) =
  "Active issue" </$$> P.indent 2 (formatFn $ logItemToText active)

formatHistory :: ([Text] -> P.Doc) -> [LogItem] -> P.Doc
formatHistory _ [] = "Work log is empty"
formatHistory formatFn items =
  let byDay = M.toAscList (indexBy (getDay . started) items)
  in foldl1 (</$$>) (formatDay formatFn <$> byDay)

formatDay :: ([Text] -> P.Doc) -> (Day, [LogItem]) -> P.Doc
formatDay formatFn (day, items) =
  let t = logItemToText <$> reverse items
      total = mconcat $ timeSpent <$> items
  in
    txt (tshow day) <> " (" <> txt (toDurationString total) <> ")" </$$>
    P.indent 2 (formatTable formatFn t)

logItemToText :: LogItem -> [Text]
logItemToText item = [tshow $ issueKey (item :: LogItem),
                      formatAsTime $ started (item :: LogItem),
                      toDurationString $ timeSpent (item :: LogItem)]
  where formatStr = "%H:%M"
        formatAsTime = T.pack . formatTimestamp defaultTimeLocale formatStr

mkFormatFn :: Text -> [[Text]] -> [Text] -> P.Doc
mkFormatFn sep rows =
  let widths = foldl (zipWithDefault max 0) [] (fmap T.length <$> rows)
  in formatRow widths sep

formatTable :: ([Text] -> P.Doc) -> [[Text]] -> P.Doc
formatTable _ []            = P.empty
formatTable formatFn [r]    = formatFn r
formatTable formatFn (r:rs) = formatFn r </$> formatTable formatFn rs

--foldl (</$>) P.empty (formatFn <$> rows)

formatRow :: [Int] -> Text -> [Text] -> P.Doc
formatRow widths sep row =
  let columns = zipWith (\n t -> T.justifyLeft n ' ' t) widths row
  in txt $ intercalate sep columns

zipWithDefault :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithDefault _ _ [] []         = []
zipWithDefault f a [] (w:ws)     = f a w:zipWithDefault f a [] ws
zipWithDefault f a (v:vs) []     = f v a:zipWithDefault f a vs []
zipWithDefault f a (v:vs) (w:ws) = f v w:zipWithDefault f a vs ws

indexBy :: Ord k => (a -> k) -> [a] -> Map k [a]
indexBy f = foldl op M.empty
  where op m x = M.insertWith (<>) (f x) [x] m

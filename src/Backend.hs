{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE StrictData            #-}

module Backend
  ( Handle(..)
  , Page(..)
  ) where

import           BasicPrelude

import           Tracker.Types

data Page = Page
  { offset :: Int
  , limit  :: Int
  }

data Handle = Handle
  { book   :: LogItem -> IO ()
  , search :: JQL -> Page -> IO [Issue]
  , fetch  :: IssueKey -> IO (Maybe Issue)
  }

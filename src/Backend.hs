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

instance Monoid Handle where
  mempty = Handle
    { book = \_ -> pure ()
    , search = \_ _ -> pure []
    , fetch = \_ -> pure Nothing
    }
  mappend left right = Handle
    { book = \item ->
        book left item >> book right item
    , search = \jql page ->
        liftM2 mappend (search left jql page) (search right jql page)
    -- Left biased fetch. Return the issue if it exists in left, otherwise
    -- return the issue from right.
    , fetch = \key -> liftM2 (<|>) (fetch left key) (fetch right key)
    }


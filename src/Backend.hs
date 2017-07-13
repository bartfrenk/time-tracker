{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE StrictData            #-}

module Backend
  ( Handle(..)
  , Page(..)
  , MonadBackend(..)
  ) where


import           BasicPrelude

import           Tracker.Types

data Page = Page
  { offset :: Int
  , limit  :: Int
  }

data Handle m = Handle
  { book   :: LogItem -> m ()
  , search :: JQL -> Page -> m [Issue]
  }


class Monad m => MonadBackend m where
  book :: LogItem -> m ()
  search :: JQL -> Page -> m [Issue]

{-# LANGUAGE NoImplicitPrelude #-}

module Console.Config where

import           BasicPrelude

data Config = Config
  { aliases        :: Map String [String]
  , defaultProject :: String
  }


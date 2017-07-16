{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Console.Config where

import           BasicPrelude
import           Data.Aeson


data Config = Config
  { aliases        :: Map Text [Text]
  }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> (words <$>) `fmap` (v .: "aliases")



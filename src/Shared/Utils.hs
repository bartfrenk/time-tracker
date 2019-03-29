module Shared.Utils where

import           BasicPrelude


import           Control.Lens         ((^?))
import           Control.Monad.Catch
import           Data.Aeson
import           Data.Aeson.Lens      (key)
import           Data.Aeson.Types     (Parser, parseEither)
import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.String.Conv     (toS)
import           Data.Yaml            (decodeEither)
import           System.Directory     (getHomeDirectory)
import           System.FilePath

breakdown :: Integer -> [Integer] -> [Integer]
breakdown n (x:xs) = (n `div` x):breakdown (n `mod` x) xs
breakdown _ []     = []

expand :: Ord a => Map a [a] -> [a] -> [a]
expand m xs =
  xs >>= (\s -> Map.findWithDefault [s] s m)

parseThrow :: MonadThrow m => (a -> Parser b) -> a -> m b
parseThrow p v = case parseEither p v of
  Left err -> throwM $ ParseError err
  Right b  -> return b

decodeThrow :: (MonadThrow m, FromJSON a) => Lazy.ByteString -> m a
decodeThrow bs = case eitherDecode bs of
  Left err -> throwM $ ParseError err
  Right a  -> return a

decodeThrowYAML :: (MonadThrow m, FromJSON a) => Strict.ByteString -> m a
decodeThrowYAML bs = case decodeEither bs of
  Left err -> throwM $ ParseError err
  Right a  -> return a

newtype ParseError = ParseError String deriving (Show, Typeable)

instance Exception ParseError

fromValueThrow :: (FromJSON a, MonadThrow m) => Value -> m a
fromValueThrow value = case fromJSON value of
  Error err -> throwM $ ParseError err
  Success a -> return a

expandFilePath :: FilePath -> IO FilePath
expandFilePath path = joinPath <$> (expandDir `mapM` splitDirectories path)
  where expandDir dir
          | dir == "~" = getHomeDirectory
          | dir == "$HOME" = getHomeDirectory
          | otherwise = return dir

fromConfig :: (FromJSON a, MonadThrow m) => Value -> Text -> m a
fromConfig config sub = case config ^? key sub of
  Nothing    -> throwM $ ParseError $ "key '" ++ toS sub ++ "' not present"
  Just value -> fromValueThrow value

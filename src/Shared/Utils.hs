module Shared.Utils where

import           Control.Monad.Catch
import           Data.Aeson
import           Data.Aeson.Types     (Parser, parseEither)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map

breakdown :: Integer -> [Integer] -> [Integer]
breakdown n (x:xs) = (n `div` x):breakdown (n `mod` x) xs
breakdown _ []     = []

expand :: Ord a => Map a [a] -> [a] -> [a]
expand m xs =
  xs >>= (\s -> Map.findWithDefault [s] s m)

parseThrow :: MonadThrow m => (a -> Parser b) -> a -> m b
parseThrow p v = case parseEither p v of
  Left _  -> undefined -- TODO
  Right b -> return b

decodeThrow :: (MonadThrow m, FromJSON a) => Lazy.ByteString -> m a
decodeThrow bs = case eitherDecode bs of
  Left _  -> undefined -- TODO
  Right a -> return a



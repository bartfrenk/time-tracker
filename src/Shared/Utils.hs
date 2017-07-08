module Shared.Utils where

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map

breakdown :: Integer -> [Integer] -> [Integer]
breakdown n (x:xs) = (n `div` x):breakdown (n `mod` x) xs
breakdown _ []     = []

expand :: Ord a => Map a [a] -> [a] -> [a]
expand m xs =
  xs >>= (\s -> Map.findWithDefault [s] s m)

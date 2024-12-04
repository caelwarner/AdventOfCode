module IterTools (counts) where

import Data.Map (Map)
import qualified Data.Map as Map

counts :: Ord a => [a] -> Map a Int
counts = foldr (\key -> Map.insertWith (+) key 1) Map.empty

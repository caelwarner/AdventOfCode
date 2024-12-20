module Main (main) where

import Read
import Run
import Vec2
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array.Unboxed
import Data.List (nub)

parse :: UArray Vec2 Char -> Map Char [Vec2]
parse contents = foldr (\(i, c) -> Map.insertWith (++) c [i]) Map.empty $ filter (\(_, c) -> c /= '.') $ assocs contents

antiNodes :: [Vec2] -> [Vec2]
antiNodes antennas = [(a - b) + a | a <- antennas, b <- antennas, a /= b]

antiNodesHarmonics :: [Vec2] -> [Vec2]
antiNodesHarmonics antennas = concat $ [antiNodesInLine a b | a <- antennas, b <- antennas, a /= b]
    where antiNodesInLine a b = fmap (\i -> ((a - b) *| i) + a) [0..25]

findAllAntiNodes :: ([Vec2] -> [Vec2]) -> UArray Vec2 Char -> Int
findAllAntiNodes f contents = length $ nub $ concatMap (filter (inRange (bounds contents)) . f) $ Map.elems $ parse contents

main :: IO ()
main = do
    contents <- inputAsArray (Year 2024) (Day 8)

    run PartOne (findAllAntiNodes antiNodes contents)
    run PartTwo (findAllAntiNodes antiNodesHarmonics contents)


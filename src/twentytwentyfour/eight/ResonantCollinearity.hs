module Main (main) where

import Read
import Run
import Vec2d
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array.Unboxed
import Data.List (nub)

parse :: UArray Vec2d Char -> Map Char [Vec2d]
parse contents = foldr (\(i, c) -> Map.insertWith (++) c [i]) Map.empty $ filter (\(_, c) -> c /= '.') $ assocs contents

antiNodes :: [Vec2d] -> [Vec2d]
antiNodes antennas = [(a - b) + a | a <- antennas, b <- antennas, a /= b]

antiNodesHarmonics :: [Vec2d] -> [Vec2d]
antiNodesHarmonics antennas = concat $ [antiNodesInLine a b | a <- antennas, b <- antennas, a /= b]
    where antiNodesInLine a b = fmap (\i -> ((a - b) *| i) + a) [0..25]

findAllAntiNodes :: ([Vec2d] -> [Vec2d]) -> UArray Vec2d Char -> Int
findAllAntiNodes f contents = length $ nub $ concatMap (filter (inRange (bounds contents)) . f) $ Map.elems $ parse contents

main :: IO ()
main = do
    contents <- inputAsArray (Year 2024) (Day 8)

    run PartOne (findAllAntiNodes antiNodes contents)
    run PartTwo (findAllAntiNodes antiNodesHarmonics contents)


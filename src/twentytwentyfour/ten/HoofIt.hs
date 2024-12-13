module Main (main) where

import Read
import Run
import Vec2d
import Data.Array.Unboxed
import Data.List.Extra (nubOrd)

trailhead :: Vec2d -> Int -> UArray Vec2d Int -> [Vec2d]
trailhead pos prev grid
    | not $ inRange (bounds grid) pos = []
    | (prev + 1) /= grid ! pos = []
    | grid ! pos == 9 = [pos]
    | otherwise = trailhead (pos + north) x grid ++ trailhead (pos + south) x grid ++ trailhead (pos + east) x grid ++ trailhead (pos + west) x grid
        where x = grid ! pos

main :: IO ()
main = do
    contents <- inputAsIntArray (Year 2024) (Day 10)

    run PartOne (sum $ fmap (\(pos, _) -> length $ nubOrd $ trailhead pos (-1) contents) $ filter (\(_, x) -> x == 0) $ assocs contents)
    run PartTwo (sum $ fmap (\(pos, _) -> length $ trailhead pos (-1) contents) $ filter (\(_, x) -> x == 0) $ assocs contents)


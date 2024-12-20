{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import Read
import Run
import Vec2
import Parse
import Data.Foldable (find)
import Data.Maybe

parse :: String -> (Vec2, Vec2)
parse line = (V2 x y, V2 dx dy)
    where [x, y, dx, dy] = extractNumbers line

step :: Int -> (Vec2, Vec2) -> Vec2
step i (pos, v) = (pos + (v *| i)) % V2 101 103

safetyFactor :: [Vec2] -> Int
safetyFactor = product . foldr quadrant [0, 0, 0, 0]

isChristmasTree :: [Vec2] -> Bool
isChristmasTree robots = ne >= l || nw >= l || se >= l || sw >= l
    where 
        [ne, nw, se, sw] = foldr quadrant [0, 0, 0, 0] robots
        l = (length robots `div` 11) * 5

findChristmasTree :: [(Vec2, Vec2)] -> Int
findChristmasTree robots = fromJust $ find (\i -> isChristmasTree $ fmap (step i) robots) [1..10403]

quadrant :: Vec2 -> [Int] -> [Int]
quadrant (V2 x y) [ne, nw, se, sw]
    | x < 50 && y < 51 = [ne + 1, nw, se, sw]
    | x > 50 && y < 51 = [ne, nw + 1, se, sw]
    | x < 50 && y > 51 = [ne, nw, se + 1, sw]
    | x > 50 && y > 51 = [ne, nw, se, sw + 1]
    | otherwise = [ne, nw, se, sw]

main :: IO ()
main = do
    contents <- inputAsStrList (Year 2024) (Day 14)

    run PartOne (safetyFactor $ step 100 . parse <$> contents)
    run PartTwo (findChristmasTree $ parse <$> contents)

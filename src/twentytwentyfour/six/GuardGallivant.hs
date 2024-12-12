module Main (main) where

import Data.Maybe
import Read
import Run
import List2d
import Vec2d
import Data.Array.Unboxed
import Data.List (nub)
import Data.Tuple.Extra
import Data.List.Extra (nubOrdOn)

parse :: Int -> Int -> [String] -> (Vec2d, UArray Vec2d Bool)
parse width height contents = (guard, walls)
    where
        guard = fromJust $ elemIndex2d '^' contents
        walls = listArray (boundsForSize width height) ((== '#') <$> concat contents)

step :: Vec2d -> Vec2d -> UArray Vec2d Bool -> [(Vec2d, Vec2d)]
step d g walls =
    case walls !? (g + d) of
        Nothing -> [(g, d)]
        Just False -> (g, d):step d (g + d) walls
        Just True -> (g, d):step (rotateRight d) g walls

doesLoop :: [(Vec2d, Vec2d)] -> [(Vec2d, Vec2d)] -> Bool
doesLoop (_:hare:x1) (tortoise:x2) = (hare == tortoise) || doesLoop x1 x2
doesLoop _ _ = False

countNewObstructions :: Vec2d -> UArray Vec2d Bool -> Int
countNewObstructions guard walls =
    length $ nub $ filter doesLoopWithObstruction $ filter validObstructionPos $ nubOrdOn (uncurry (+)) path
    where
        path = step north guard walls
        validObstructionPos (g, d) = (walls !? (g + d)) == Just False && ((g + d) /= guard)
        doesLoopWithObstruction (g, d) = uncurry doesLoop $ dupe $ step north guard $ walls // [(g + d, True)]

main :: IO ()
main = do
    contents <- inputAsStrList (Year 2024) (Day 6)
    let w = length $ head contents
    let h = length contents

    run PartOne (length $ nub $ fmap fst $ uncurry (step north) $ parse w h contents)
    run PartTwo (uncurry countNewObstructions $ parse w h contents)
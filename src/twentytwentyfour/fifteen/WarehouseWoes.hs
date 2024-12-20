{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import Read
import Run
import Vec2
import List2d
import Data.Array.Unboxed
import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe

parse :: [String] -> ([Char], Vec2, UArray Vec2 Char)
parse contents = (concat moves, pos, listArray (boundsForSize (length $ head grid) (length grid)) $ replace "@" "." $ concat grid)
    where
        pos = fromJust $ elemIndex2d '@' grid
        [grid, moves] = split (== "") contents

parseWide :: [String] -> ([Char], Vec2, UArray Vec2 Char)
parseWide contents = (concat moves, pos, listArray (boundsForSize (length (head grid) * 2) (length grid)) $ replace "O" "[]" $ replace "#" "##" $ replace "." ".." $ replace "@" "." $ concat grid)
    where
        pos = fromJust (elemIndex2d '@' grid) * V2 2 1
        [grid, moves] = split (== "") contents

type MoveBoxesF = Vec2 -> Vec2 -> UArray Vec2 Char -> Maybe [(Vec2, Char)]

move :: MoveBoxesF -> MoveBoxesF -> [Char] -> Vec2 -> UArray Vec2 Char -> UArray Vec2 Char
move _ _ [] _ grid = grid
move fV fH (instruction:t) pos grid
    | next == '#' = move fV fH t pos grid
    | (next == '[' || next == ']' || next == 'O') && isVertical dir = moveBoxes fV
    | (next == '[' || next == ']' || next == 'O') && isHorizontal dir = moveBoxes fH
    | otherwise = move fV fH t (pos + dir) grid
    where
        moveBoxes f = case f pos dir grid of
            Just l -> move fV fH t (pos + dir) $ grid // sortOn snd l
            Nothing -> move fV fH t pos grid
        next = grid ! (pos + dir)
        dir = case instruction of
            '>' -> east
            '<' -> west
            '^' -> north
            'v' -> south

moveBoxesH :: Vec2 -> Vec2 -> UArray Vec2 Char -> Maybe [(Vec2, Char)]
moveBoxesH pos dir grid =
    case (grid ! pos, grid ! (pos + dir)) of
        (_, '#') -> Nothing
        (c, '.') -> Just [(pos, '.'), (pos + dir, c)]
        (c, _) -> ((pos, '.'):) . ((pos + dir, c):) <$> moveBoxesH (pos + dir) dir grid

moveBoxesV :: Vec2 -> Vec2 -> UArray Vec2 Char -> Maybe [(Vec2, Char)]
moveBoxesV pos dir grid =
    case (grid ! pos, grid ! (pos + dir)) of
        (_, '#') -> Nothing
        (c, '.') -> Just [(pos, '.'), (pos + dir, c)]
        (']', ']') -> ((pos, '.'):) . ((pos + dir, ']'):) <$> moveBoxesV (pos + dir) dir grid
        ('[', '[') -> ((pos, '.'):) . ((pos + dir, '['):) <$> moveBoxesV (pos + dir) dir grid
        (c, '[') -> ((pos, '.'):) . ((pos + dir, c):) <$> ((++) <$> moveBoxesV (pos + dir) dir grid <*> moveBoxesV (pos + dir + east) dir grid)
        (c, ']') -> ((pos, '.'):) . ((pos + dir, c):) <$> ((++) <$> moveBoxesV (pos + dir) dir grid <*> moveBoxesV (pos + dir + west) dir grid)

calculateGPSCoords :: Char -> UArray Vec2 Char -> Int
calculateGPSCoords c = sum . map (\(V2 x y, _) -> y * 100 + x) . filter (\(_, e) -> e == c) . assocs

main :: IO ()
main = do
    contents <- inputAsStrList (Year 2024) (Day 15)

    run PartOne (calculateGPSCoords 'O' $ uncurry3 (move moveBoxesH moveBoxesH) $ parse contents)
    run PartTwo (calculateGPSCoords '[' $ uncurry3 (move moveBoxesV moveBoxesH) $ parseWide contents)

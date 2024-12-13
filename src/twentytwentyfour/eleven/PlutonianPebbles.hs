module Main (main) where

import Read
import Run
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

blink :: Int -> Int -> MultiSet Int -> MultiSet Int
blink i end pebbles 
    | i >= end = pebbles 
    | otherwise = blink (i + 1) end $ MultiSet.unions $ fmap updatePebble $ MultiSet.toOccurList pebbles

updatePebble :: (Int, Int) -> MultiSet Int
updatePebble (0, n) = MultiSet.fromOccurList [(1, n)]
updatePebble (x, n) = 
    let x' = show x in
    if even $ length x' then
        let (h, t) = splitAt (length x' `div` 2) x' in
        MultiSet.fromOccurList [(read h, n), (read t, n)] 
    else
        MultiSet.fromOccurList [(x * 2024, n)]

main :: IO ()
main = do
    contents <- inputAsStr (Year 2024) (Day 11)

    run PartOne (MultiSet.size $ blink 0 25 $ MultiSet.fromList $ fmap read $ words contents)
    run PartTwo (MultiSet.size $ blink 0 75 $ MultiSet.fromList $ fmap read $ words contents)

module Main (main) where

import Read
import Run
import ListTools

parse :: [String] -> [[Int]]
parse = fmap (fmap read . words)

isSafe :: [Int] -> Bool
isSafe report = isSafe' report || isSafe' (reverse report)
    where isSafe' = all (\x -> x >= 1 && x <= 3) . differences

safeReports :: [[Int]] -> Int
safeReports = length . filter isSafe

safeReportsWithDamper :: [[Int]] -> Int
safeReportsWithDamper = length . filter (any isSafe) . fmap comb

comb :: [Int] -> [[Int]]
comb l = [removeAt i l | i <- [0..(length l - 1)]]

main :: IO ()
main = do
    contents <- inputAsStrList (Year 2024) (Day 2)

    run PartOne (safeReports $ parse contents)
    run PartTwo (safeReportsWithDamper $ parse contents)

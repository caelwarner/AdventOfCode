module Main (main) where

import Data.List.Extra
import Data.Tuple.Extra (both)
import Read
import Run
import Data.Maybe
import Control.Arrow

type Rule = (Int, Int)
type Update = [Int]

parse :: [String] -> ([Rule], [Update])
parse contents = (rules', updates')
    where
        (rules, updates) = fromJust $ stripInfix [""] contents
        rules' = fmap (both read . second tail . splitAt 2) rules
        updates' = fmap (fmap read . splitOn ",") updates

isCorrectlyOrdered :: [Rule] -> Update -> Bool
isCorrectlyOrdered rules = all checkIfBefore . tails . reverse
    where
        checkIfBefore [] = True
        checkIfBefore (h:t) = all (\x -> isNothing $ find (\(a, b) -> a == h && b == x) rules) t

middlePagesOfCorrectUpdates :: [Rule] -> [Update] -> Int
middlePagesOfCorrectUpdates rules = sum . fmap (\x -> x !! (length x `div` 2)) . filter (isCorrectlyOrdered rules)

middlePagesOfIncorrectUpdates :: [Rule] -> [Update] -> Int
middlePagesOfIncorrectUpdates rules = sum . fmap ((\x -> x !! (length x `div` 2)) . sortBy (customSort rules)) . filter (not . isCorrectlyOrdered rules)

customSort :: [Rule] -> Int -> Int -> Ordering
customSort rules a b = if (a, b) `elem` rules then GT else LT

main :: IO ()
main = do
    contents <- inputAsStrList (Year 2024) (Day 5)

    run PartOne (uncurry middlePagesOfCorrectUpdates $ parse contents)
    run PartTwo (uncurry middlePagesOfIncorrectUpdates $ parse contents)

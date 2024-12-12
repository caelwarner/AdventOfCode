module Main (main) where

import Data.List.Extra
import Run
import Read

parseLine :: String -> ([Int], Int)
parseLine line = (read <$> words (tail inputs), read output)
    where (output, inputs) = breakOn ":" line

isValid :: (Int -> Int) -> [Int] -> Int -> Bool
isValid _ [] _ = error "List shouldn't be empty"
isValid f [ih] o = f ih == o
isValid f (ih:it) o = isValid (+i) it o || isValid (*i) it o
    where i = f ih

isValidWithConcat :: (Int -> Int) -> [Int] -> Int -> Bool
isValidWithConcat _ [] _ = error "List shouldn't be empty"
isValidWithConcat f [ih] o = f ih == o
isValidWithConcat f (ih:it) o = isValidWithConcat (+i) it o || isValidWithConcat (*i) it o || isValidWithConcat (concatInt i) it o
    where
        i = f ih
        concatInt a b = read $ show a ++ show b

main :: IO ()
main = do
    contents <- inputAsStrList (Year 2024) (Day 7)

    run PartOne (sum $ fmap snd $ filter (uncurry $ isValid (+0)) $ fmap parseLine contents)
    run PartTwo (sum $ fmap snd $ filter (uncurry $ isValidWithConcat (+0)) $ fmap parseLine contents)

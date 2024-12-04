module Main (main) where
import Data.List.Extra
import Data.Tuple.Extra
import qualified Data.MultiSet as MultiSet
import Run
import Read

parse :: [String] -> ([Int], [Int])
parse = unzip . fmap (both read . word1)

totalDistance :: ([Int], [Int]) -> Int
totalDistance = sum . fmap (abs . uncurry (-)) . uncurry zip . both sort

similarityScore :: ([Int], [Int]) -> Int
similarityScore contents = sum . fmap (\x -> x * MultiSet.occur x set) $ fst contents
    where set = MultiSet.fromList $ snd contents

main :: IO ()
main = do
    contents <- inputAsStrList (Year 2024) (Day 1)

    run PartOne (totalDistance $ parse contents)
    run PartTwo (similarityScore $ parse contents)

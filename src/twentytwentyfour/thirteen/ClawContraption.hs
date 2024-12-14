module Main (main) where

import Read
import Run
import Parse
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Data.List.Extra
import Data.Either.Extra

parse :: [String] -> [Matrix Double]
parse contents = (\[a, b, o] -> Matrix.transpose $ Matrix.fromList 3 2 $ extractNumbers a ++ extractNumbers b ++ extractNumbers o) <$> split (== "") contents

tokens :: Matrix Double -> Int
tokens m = if (aF <= 0.005 || aF >= 0.995) && (bF <= 0.005 || bF >= 0.995) then a * 3 + b else 0
    where 
        m' = fromRight' $ Matrix.rref m
        (aI, aF) = properFraction $ Matrix.getElem 1 3 m'
        (bI, bF) = properFraction $ Matrix.getElem 2 3 m'
        a = if aF >= 0.995 then aI + 1 else aI
        b = if bF >= 0.995 then bI + 1 else bI

add10Trillion :: Matrix Double -> Matrix Double
add10Trillion = Matrix.mapCol (\_ e -> e + 10000000000000.0) 3 

main :: IO ()
main = do
    contents <- inputAsStrList (Year 2024) (Day 13)

    run PartOne (sum $ tokens <$> parse contents)
    run PartTwo (sum $ tokens . add10Trillion <$> parse contents)

{-# LANGUAGE InstanceSigs #-}
module Run (Part(..), run) where

import System.Clock
import Control.DeepSeq

data Part = PartOne | PartTwo

instance Show Part where
    show :: Part -> String
    show PartOne = "Part 1"
    show PartTwo = "Part 2"

run :: (Show a, NFData a) => Part -> a -> IO ()
run part a = do
    start <- getTime Monotonic
    let out = a
    end <- a `deepseq` getTime Monotonic
    let diff = fromIntegral (toNanoSecs (diffTimeSpec start end)) / 1e6 :: Double

    putStrLn $ "\x1b[0;1m" ++ show part ++ ": \x1b[0;32m" ++ show out ++ "\x1b[0;30m | \x1b[0;1mTime elapsed: " ++ show diff ++ "ms\x1b[0m"

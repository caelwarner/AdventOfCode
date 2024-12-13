{-# LANGUAGE TupleSections #-}
module Main (main) where

import Run
import Read
import Data.Char
import Data.Array.Unboxed

-- data File = File Int Int Int deriving Show

expand :: [Char] -> UArray Int Int
expand contents = listArray (0, length blocks - 1) blocks
    where
        blocks = expand' 0 $ fmap digitToInt contents
        expand' i [file] = replicate file i
        expand' i (file:free:c) = replicate file i ++ replicate free (-1) ++ expand' (i + 1) c
        expand' _ _ = []

fragment :: Int -> Int -> UArray Int Int -> UArray Int Int
fragment start end blocks
    | end <= start = blocks
    | blocks ! start /= (-1) = fragment (start + 1) end blocks
    | blocks ! end == (-1) = fragment start (end - 1) blocks
    | otherwise = fragment (start + 1) (end - 1) $ blocks // [(start, blocks ! end), (end, -1)]

checksum :: (Int -> Int -> UArray Int Int -> UArray Int Int) -> UArray Int Int -> Int
checksum f blocks = sum $ fmap (uncurry (*)) $ filter (\(_, a) -> a /= (-1)) $ assocs $ f start end blocks
    where (start, end) = bounds blocks

freeSpace :: Int -> Int -> UArray Int Int -> UArray Int Int
freeSpace _ end blocks
    | end <= 0 = blocks
    | blocks ! end == (-1) = freeSpace 0 (end - 1) blocks
    | otherwise =
        if null empty then
            freeSpace 0 (end - length file) blocks
        else
            freeSpace 0 (end - 1) $ blocks // (fmap (, fid) empty ++ fmap (, -1) file)
        where
            fid = blocks ! end
            file = sizeOfBlock fid end blocks
            empty = findEmptyBlock 0 (length file) end blocks


sizeOfBlock :: Int -> Int -> UArray Int Int -> [Int]
sizeOfBlock e i blocks
    | i < 0 = []
    | blocks ! i == e = i:sizeOfBlock e (i - 1) blocks
    | otherwise = []

findEmptyBlock :: Int -> Int -> Int -> UArray Int Int -> [Int]
findEmptyBlock i size end blocks
    | i >= end = []
    | blocks ! i /= (-1) = findEmptyBlock (i + 1) size end blocks
    | otherwise =
        let block = sizeOfBlock (-1) i blocks in
        if length block == size then block else findEmptyBlock (i + 1) size end blocks

-- parse :: Int -> [Char] -> Seq File
-- parse i [file] = Seq.singleton (File i (digitToInt file) 0)
-- parse i (file:free:c) = File i (digitToInt file) (digitToInt free) <| parse (i + 1) c
-- parse _ _ = Seq.empty

-- freeSpace :: Int -> Seq File -> Seq File
-- freeSpace end files 
--     | end >= 1 =
--         let (File idN file free) = fromJust $ Seq.lookup end files in
--         let freeI = findFreeSpace 0 end file files in

--         case freeI of 
--             Just i -> 
--                 let (File idN' file' free') = fromJust $ Seq.lookup i files in
--                 freeSpace end
--                 $ Seq.update i (File idN' file' 0) 
--                 $ Seq.insertAt (i + 1) (File idN file (free' - file)) 
--                 $ Seq.deleteAt end
--                 $ Seq.adjust' (\(File idN'' file'' free'') -> File idN'' file'' (free'' + file + free)) (end - 1) files
--             Nothing -> freeSpace (end - 1) files
--     | otherwise = files

-- findFreeSpace :: Int -> Int -> Int -> Seq File -> Maybe Int
-- findFreeSpace i end size files = do
--     (File _ _ free) <- Seq.lookup i files
--     if free >= size then 
--         if i >= end then Nothing else return i 
--     else 
--         findFreeSpace (i + 1) end size files

-- checksum' :: Seq File -> Int
-- checksum' files = snd $ foldl checkSumForFile (0, 0) $ freeSpace (length files - 1) files
--     where checkSumForFile (i, s) (File idN file free)  = (i + file + free, s + (idN * (file * (i + i + file - 1)) `div` 2))

main :: IO ()
main = do
    contents <- inputAsStr (Year 2024) (Day 9)

    run PartOne (checksum fragment $ expand contents)
    run PartTwo (checksum freeSpace $ expand contents)

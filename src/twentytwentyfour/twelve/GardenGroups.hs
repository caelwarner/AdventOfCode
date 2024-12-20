module Main (main) where

import Read
import Run
import Vec2
import Data.Array.Unboxed
import Data.Set (Set)
import qualified Data.Set as Set

findRegion :: Vec2 -> Char -> UArray Vec2 Char -> Set Vec2 -> Set Vec2
findRegion pos c grid region
    | not (inRange (bounds grid) pos) = region
    | grid ! pos /= c = region
    | Set.member pos region = region
    | otherwise =
        let r = Set.insert pos region in
        foldr (\p r' -> findRegion p c grid r') r $ around pos

perimiter :: Set Vec2 -> UArray Vec2 Char -> Int
perimiter region grid = sum $ length . filter (\p -> grid !? p /= Just c) . around <$> Set.toList region
    where c = grid ! Set.elemAt 0 region

sides :: Set Vec2 -> UArray Vec2 Char -> Int
sides region grid = sum $ (\p -> length $ filter (\i -> isOutsideCorner p i || isInsideCorner p i) [0..3]) <$> Set.toList region
    where 
        c = grid ! Set.elemAt 0 region
        isOutsideCorner p i = grid !? (p + rotateByR i east) /= Just c && grid !? (p + rotateByR i north) /= Just c
        isInsideCorner p i = grid !? (p + rotateByR i east) == Just c && grid !? (p + rotateByR i north) == Just c && grid !? (p + rotateByR i northEast) /= Just c

allRegions :: UArray Vec2 Char -> [Set Vec2]
allRegions grid = foldr addRegion [] $ indices grid
    where addRegion pos regions = if any (Set.member pos) regions then regions else findRegion pos (grid ! pos) grid Set.empty:regions

totalPrice :: UArray Vec2 Char -> [Set Vec2] -> Int
totalPrice grid = sum . fmap (\r -> Set.size r * perimiter r grid)

totalDiscountedPrice :: UArray Vec2 Char -> [Set Vec2] -> Int
totalDiscountedPrice grid = sum . fmap (\r -> Set.size r * sides r grid)

main :: IO ()
main = do
    contents <- inputAsArray (Year 2024) (Day 12)

    run PartOne (totalPrice contents $ allRegions contents)
    run PartTwo (totalDiscountedPrice contents $ allRegions contents)

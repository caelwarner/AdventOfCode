{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module List2d (elemIndexes2d, elemIndex2d) where

import Vec2d
import Control.Monad
import Data.Maybe
import Data.List (find)

elemIndexes2d :: (Eq a) => a -> [[a]] -> [Vec2d]
elemIndexes2d e list2d = do
    (y, row) <- zip [0..] list2d
    (x, a) <- zip [0..] row

    guard (a == e)
    return $ V2 x y

elemIndex2d :: (Eq a) => a -> [[a]] -> Maybe Vec2d
elemIndex2d e list2d = fmap (\(y, Just (x, _)) -> V2 x y) $ find (\(_, row) -> isJust row) $ fmap elemInRow $ zip [0..] list2d
    where elemInRow (y, row) = (y, find (\(_, a) -> a == e) (zip [0..] row))

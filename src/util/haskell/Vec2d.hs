{-# LANGUAGE InstanceSigs #-}
module Vec2d (Vec2d(..), north, south, east, west, rotateLeft, rotateRight, boundsForSize) where

import Data.Ix

data Vec2d = Vec2d Int Int deriving (Eq, Ord)

north :: Vec2d
north = Vec2d 0 (-1)

south :: Vec2d
south = Vec2d 0 1

east :: Vec2d
east = Vec2d (-1) 0

west :: Vec2d
west = Vec2d 1 0

rotateLeft :: Vec2d -> Vec2d
rotateLeft (Vec2d x y) = Vec2d y (-x)

rotateRight :: Vec2d -> Vec2d
rotateRight (Vec2d x y) = Vec2d (-y) x

boundsForSize :: Int -> Int -> (Vec2d, Vec2d)
boundsForSize w h = (Vec2d 0 0, Vec2d (w - 1) (h - 1))

instance Num Vec2d where
    (+) :: Vec2d -> Vec2d -> Vec2d
    (+) (Vec2d ax ay) (Vec2d bx by) = Vec2d (ax + bx) (ay + by)
    (*) :: Vec2d -> Vec2d -> Vec2d
    (*) (Vec2d ax ay) (Vec2d bx by) = Vec2d (ax * bx) (ay * by)
    abs (Vec2d x y) = Vec2d (abs x) (abs y)
    signum (Vec2d x y) = Vec2d (signum x) (signum y)
    fromInteger a = Vec2d (fromInteger a) (fromInteger a) 
    negate (Vec2d x y) = Vec2d (negate x) (negate y)

instance Ix Vec2d where
    range (Vec2d ax ay, Vec2d bx by) = [Vec2d x y | y <- [ay..by], x <- [ax..bx]]
    index (Vec2d ax ay, Vec2d bx _) (Vec2d x y) = (x - ax) + ((y - ay) * ((bx + 1) - ax))
    inRange (Vec2d ax ay, Vec2d bx by) (Vec2d x y) = x >= ax && x <= bx && y >= ay && y <= by

instance Show Vec2d where
    show (Vec2d x y) = "<" ++ show x ++ " " ++ show y ++ ">"

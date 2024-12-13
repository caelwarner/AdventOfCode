{-# LANGUAGE DeriveGeneric #-}
module Vec2d (Vec2d(..), north, south, east, west, rotateLeft, rotateRight, boundsForSize, (|*), (*|)) where

import Data.Ix
import Control.DeepSeq
import GHC.Generics

data Vec2d = V2 Int Int deriving (Eq, Ord, Generic)

north :: Vec2d
north = V2 0 (-1)

south :: Vec2d
south = V2 0 1

east :: Vec2d
east = V2 (-1) 0

west :: Vec2d
west = V2 1 0

rotateLeft :: Vec2d -> Vec2d
rotateLeft (V2 x y) = V2 y (-x)

rotateRight :: Vec2d -> Vec2d
rotateRight (V2 x y) = V2 (-y) x

boundsForSize :: Int -> Int -> (Vec2d, Vec2d)
boundsForSize w h = (V2 0 0, V2 (w - 1) (h - 1))

(|*) :: Int -> Vec2d -> Vec2d
(|*) a (V2 x y) = V2 (x * a) (y * a)

(*|) :: Vec2d -> Int -> Vec2d
(*|) (V2 x y) a = V2 (x * a) (y * a)

instance Num Vec2d where
    (+) (V2 ax ay) (V2 bx by) = V2 (ax + bx) (ay + by)
    (*) (V2 ax ay) (V2 bx by) = V2 (ax * bx) (ay * by)
    abs (V2 x y) = V2 (abs x) (abs y)
    signum (V2 x y) = V2 (signum x) (signum y)
    fromInteger a = V2 (fromInteger a) (fromInteger a) 
    negate (V2 x y) = V2 (negate x) (negate y)

instance Ix Vec2d where
    range (V2 ax ay, V2 bx by) = [V2 x y | y <- [ay..by], x <- [ax..bx]]
    index (V2 ax ay, V2 bx _) (V2 x y) = (x - ax) + ((y - ay) * ((bx + 1) - ax))
    inRange (V2 ax ay, V2 bx by) (V2 x y) = x >= ax && x <= bx && y >= ay && y <= by

instance Show Vec2d where
    show (V2 x y) = "<" ++ show x ++ " " ++ show y ++ ">"

instance NFData Vec2d

{-# LANGUAGE DeriveGeneric #-}
module Vec2 (Vec2(..), north, south, east, west, northEast, northWest, southEast, southWest, cardinal, around, rotateL, rotateR, rotateByR, boundsForSize, isVertical, isHorizontal, (|*), (*|), (|%), (%|), (%)) where

import Data.Ix
import Control.DeepSeq
import GHC.Generics

data Vec2 = V2 Int Int deriving (Eq, Ord, Generic)

north :: Vec2
north = V2 0 (-1)

south :: Vec2
south = V2 0 1

east :: Vec2
east = V2 1 0

west :: Vec2
west = V2 (-1) 0

northEast :: Vec2
northEast = V2 1 (-1)

northWest :: Vec2
northWest = V2 (-1) (-1)

southEast :: Vec2
southEast = V2 1 1

southWest :: Vec2
southWest = V2 (-1) 1

cardinal :: [Vec2]
cardinal = [north, south, east, west]

around :: Vec2 -> [Vec2]
around v = fmap (+v) cardinal

rotateL :: Vec2 -> Vec2
rotateL (V2 x y) = V2 y (-x)

rotateR :: Vec2 -> Vec2
rotateR (V2 x y) = V2 (-y) x

rotateByR :: Int -> Vec2 -> Vec2
rotateByR i v = rotate' (i `mod` 4)
    where
        rotate' 0 = v
        rotate' 1 = rotateR v
        rotate' 2 = negate v
        rotate' 3 = rotateL v
        rotate' _ = error "number of rotations cannot be higher than 3"

boundsForSize :: Int -> Int -> (Vec2, Vec2)
boundsForSize w h = (V2 0 0, V2 (w - 1) (h - 1))

isVertical :: Vec2 -> Bool
isVertical (V2 _ y) = y /= 0

isHorizontal :: Vec2 -> Bool
isHorizontal (V2 x _) = x /= 0

(|*) :: Int -> Vec2 -> Vec2
(|*) a (V2 x y) = V2 (x * a) (y * a)

(*|) :: Vec2 -> Int -> Vec2
(*|) (V2 x y) a = V2 (x * a) (y * a)

(|%) :: Int -> Vec2 -> Vec2
(|%) a (V2 x y) = V2 (x `mod` a) (y `mod` a)

(%|) :: Vec2 -> Int -> Vec2
(%|) (V2 x y) a = V2 (x `mod` a) (y `mod` a)

(%) :: Vec2 -> Vec2 -> Vec2
(%) (V2 ax ay) (V2 bx by) = V2 (ax `mod` bx) (ay `mod` by)

instance Num Vec2 where
    (+) (V2 ax ay) (V2 bx by) = V2 (ax + bx) (ay + by)
    (*) (V2 ax ay) (V2 bx by) = V2 (ax * bx) (ay * by)
    abs (V2 x y) = V2 (abs x) (abs y)
    signum (V2 x y) = V2 (signum x) (signum y)
    fromInteger a = V2 (fromInteger a) (fromInteger a) 
    negate (V2 x y) = V2 (negate x) (negate y)

instance Ix Vec2 where
    range (V2 ax ay, V2 bx by) = [V2 x y | y <- [ay..by], x <- [ax..bx]]
    index (V2 ax ay, V2 bx _) (V2 x y) = (x - ax) + ((y - ay) * ((bx + 1) - ax))
    inRange (V2 ax ay, V2 bx by) (V2 x y) = x >= ax && x <= bx && y >= ay && y <= by

instance Show Vec2 where
    show (V2 x y) = "<" ++ show x ++ " " ++ show y ++ ">"

instance NFData Vec2

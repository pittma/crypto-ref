module Shake where

import Data.Bits
import Data.List (sortOn)

import Util

-- | XOR sheets, where a sheet is the nth item in each inner
-- | array.
theta1_ :: (Bits w, Num w) => [[w]] -> [w]
theta1_ state
  | length state /= 5 = unreachable
  | all (\x -> length x /= 5) state = unreachable
  | otherwise = map (foldr xor 0) (transpose state)

-- | XOR neighbors of the current index.
theta2_ :: (Bits w) => [w] -> [w]
theta2_ cols
  | length cols /= 5 = unreachable
  | otherwise = go cols 0
  where
    go _ 5 = []
    go c i =
      let idx1 = (i - 1) `mod` 5
          idx2 = (i + 1) `mod` 5
       in ((c !! idx1) `xor` (c !! idx2)) : go c (i + 1)

-- | XOR each inner-array element with the columns we've been working
-- | on.
theta3_ :: (Bits w) => [[w]] -> [w] -> [[w]]
theta3_ is cols = map (zipWith xor cols) is

theta :: (Bits w, Num w) => [[w]] -> [[w]]
theta state = theta3_ state $ theta2_ $ theta1_ state

offset_ :: (Integral w) => w -> w -> w
offset_ w t = (negate (t + 1) * (t + 2) `div` 2) `mod` w

coords_ :: (Integral w) => w -> w -> (w, w)
coords_ x y = (y, ((2 * x) + (3 * y)) `mod` 5)

coordsToOffsets_ :: [Int]
coordsToOffsets_ = 0 : map snd (sortOn fst $ go (0, 1) 0)
  where
    go _ 24 = []
    go (x, y) i = ((x, y), offset_ 8 i) : go (coords_ x y) (i + 1) 

rho :: (Bits w, Integral w) => [[w]] -> [[w]]
rho state =
  let offsets = coordsToOffsets_
   in outer offsets state
  where
    outer _ [] = []
    outer (a:b:c:d:e:off) (s:st) =
      zipWith rotateL s [a, b, c, d, e] : outer off st
    outer _ _ = unreachable

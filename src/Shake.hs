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
rho = outer coordsToOffsets_
  where
    outer _ [] = []
    outer (a:b:c:d:e:off) (s:st) =
      zipWith rotateL s [a, b, c, d, e] : outer off st
    outer _ _ = unreachable

pi :: [[w]] -> [[w]]
pi [] = []
pi state = go 0
  where
    go 5 = []
    go i =
      [ state !! (mkX 0 i) !! 0
      , state !! (mkX 1 i) !! 1
      , state !! (mkX 2 i) !! 2
      , state !! (mkX 3 i) !! 3
      , state !! (mkX 4 i) !! 4
      ]
        : go (i + 1)
    mkX x y = (x + (3 * y)) `mod` 5

chiTrans_ :: (Bits w) => [[w]] -> Int -> Int -> w
chiTrans_ state x y =
  let a = state !! x !! y
      a1 = state !! ((x + 1) `mod` 5) !! y
      a2 = state !! ((x + 2) `mod` 5) !! y
   in a `xor` (complement a1 .&. a2)

chi :: (Bits w) => [[w]] -> [[w]]
chi state = outer state 0
  where
    outer _ 5 = []
    outer st x = inner st x 0 : outer st (x + 1)
    inner _ _ 5 = []
    inner st' x y = chiTrans_ st' x y : inner st' x (y + 1)

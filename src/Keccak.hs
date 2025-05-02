module Keccak where
import Prelude hiding (pi, round)

import Data.Bits
import Data.List (sortOn)
import Data.Word

import Util hiding (matrify)

import Debug.Trace

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
       in ((c !! idx1) `xor` (c !! idx2 `rotateL` 1)) : go c (i + 1)

-- | XOR each inner-array element with the columns we've been working
-- | on.
theta3_ :: (Bits w) => [[w]] -> [w] -> [[w]]
theta3_ is cols = map (zipWith xor cols) is

theta :: (Bits w, Num w) => [[w]] -> [[w]]
theta state = theta3_ state $ theta2_ $ theta1_ state

offset_ :: (Integral w) => w -> w -> w
offset_ w t = ((t + 1) * (t + 2) `div` 2) `mod` w

coords_ :: Int -> Int -> (Int, Int)
coords_ x y = (y, ((2 * x) + (3 * y)) `mod` 5)

coordsToOffsets_ :: Int -> [Int]
coordsToOffsets_ w = 0 : map snd (sortOn fst (go (1, 0) 0))
  where
    go _ 24 = []
    go (x, y) i = ((x, y), offset_ w i) : go (coords_ x y) (i + 1) 

rho :: (Bits w, Integral w) => Int -> [[w]] -> [[w]]
rho width state =
  transpose . matrify $ zipWith rotateL (concat state) (coordsToOffsets_ width) 
                

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

  
lfsr_ :: Int -> Word8 -> Bool
lfsr_ 0 state = state .&. 0x01 == 1
lfsr_ c state =
  let state' =
        if state .&. 0x80 /= 0
          then (state `shiftL` 1) `xor` 0x71
          else state `shiftL` 1
   in lfsr_ (c - 1) state'

rc_ :: Int -> Bool
rc_ count
  | count == 0 = True
  | otherwise = lfsr_ count 1

mkRC_ :: (Bits w, Num w) => Int -> Int -> w -> w
mkRC_ log2 rd rc = go 0 rd rc
  where
    go j r rc
      | j == log2 + 1 = rc
      | otherwise =
        let rc' =
              if rc_ (j + (7 * r))
                then rc .|. (1 `shiftL` ((1 `shiftL`j) - 1))
                else rc
         in go (j + 1) r rc'
  
iota :: (Bits w, Num w, Show w) => Int -> Int -> [[w]] -> [[w]]
iota log2 round ((i:inner):outer) = (i `xor` mkRC_ log2 round 0 : inner) : outer
iota _ _ _ = unreachable

rnd :: (Bits w, Num w, Integral w, Show w) => Int -> Int -> Int -> [[w]] -> [[w]]
rnd l width i = iota l i . chi . pi . rho width . theta

matrify :: [w] -> [[w]]
matrify s = transpose (go s)
  where
    go [] = []
    go (s1:s2:s3:s4:s5:ss) = [s1, s2, s3, s4, s5] : go ss
    go _ = unreachable

unmatrify :: [[w]] -> [w]
unmatrify s = concat (transpose s)

keccak_f :: (Bits w, Num w, Integral w, Show w) => Int -> Int -> [w] -> [w]
keccak_f l width input = unmatrify $ run 0 (matrify input)
  where
    run r s
      | r == (12 + (2 * l) - 1) = rnd l width r s
      | otherwise = run (r + 1) (rnd l width r s)

keccak :: [Word64] -> [Word64]
keccak = keccak_f 6 64

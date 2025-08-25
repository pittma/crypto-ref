module Keccak where
import Prelude hiding (pi, round)

import Data.Bits
import Data.List (sortOn)
import Data.Word

import Util

-- | XOR sheets, where a sheet is the nth item in each inner
-- | array.
theta1_ :: [[Word64]] -> [Word64]
theta1_ state
  | length state /= 5 = unreachable
  | all (\x -> length x /= 5) state = unreachable
  | otherwise = map (foldr xor 0) (transpose state)

-- | XOR neighbors of the current index.
theta2_ :: [Word64] -> [Word64]
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
theta3_ :: [[Word64]] -> [Word64] -> [[Word64]]
theta3_ is cols = map (zipWith xor cols) is

theta :: [[Word64]] -> [[Word64]]
theta state = theta3_ state $ theta2_ $ theta1_ state

offset_ :: Int -> Int
offset_ t = ((t + 1) * (t + 2) `div` 2) `mod` 64

coords_ :: Int -> Int -> (Int, Int)
coords_ x y = (y, ((2 * x) + (3 * y)) `mod` 5)

coordsToOffsets_ :: [Int]
coordsToOffsets_ = 0 : map snd (sortOn fst (map swap (gen (1, 0) 0)))
  where
    gen _ 24 = []
    gen (x, y) i = ((x, y), offset_ i) : gen (coords_ x y) (i + 1)
    swap ((x, y), o) = ((y, x), o)

rho :: [[Word64]] -> [[Word64]]
rho state =
  matrify_ $ zipWith rotateL (concat state) (coordsToOffsets_) 
                

pi :: [[Word64]] -> [[Word64]]
pi [] = []
pi state = go 0
  where
    go 5 = []
    go i =
      [ state !! 0 !! (mkX 0 i)
      , state !! 1 !! (mkX 1 i)
      , state !! 2 !! (mkX 2 i)
      , state !! 3 !! (mkX 3 i)
      , state !! 4 !! (mkX 4 i)
      ]
        : go (i + 1)
    mkX x y = (x + (3 * y)) `mod` 5

chiTrans_ :: [[Word64]] -> Int -> Int -> Word64
chiTrans_ state x y =
  let a = state !! y !! x
      a1 = state !! y !! ((x + 1) `mod` 5)
      a2 = state !! y !! ((x + 2) `mod` 5)
   in a `xor` (complement a1 .&. a2)

chi :: [[Word64]] -> [[Word64]]
chi state = outer state 0
  where
    outer _ 5 = []
    outer st x = inner st x 0 : outer st (x + 1)
    inner _ _ 5 = []
    inner st' x y = chiTrans_ st' y x : inner st' x (y + 1)

  
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

mkRC_ :: Int -> Word64 -> Word64
mkRC_ rd rc = go 0 rd rc
  where
    go j r rc
      | j == 7 = rc
      | otherwise =
        let rc' =
              if rc_ (j + (7 * r))
                then rc .|. (1 `shiftL` ((1 `shiftL`j) - 1))
                else rc
         in go (j + 1) r rc'
  
iota :: Int -> [[Word64]] -> [[Word64]]
iota round ((i:inner):outer) = (i `xor` mkRC_ round 0 : inner) : outer
iota _ _ = unreachable

rnd :: Int -> [[Word64]] -> [[Word64]]
rnd i = iota i . chi . pi . rho . theta

matrify_ :: [w] -> [[w]]
matrify_ s = go s
  where
    go [] = []
    go (s1:s2:s3:s4:s5:ss) = [s1, s2, s3, s4, s5] : go ss
    go _ = unreachable

unmatrify_ :: [[Word64]] -> [Word64]
unmatrify_ s = concat (s)

keccak_p :: [Word64] -> [Word64]
keccak_p input = unmatrify_ $ run 0 (matrify_ input)
  where
    run r s
      | r == (12 + (2 * 6) - 1) = rnd r s
      | otherwise = run (r + 1) (rnd r s)

toWord64 :: [Word8] -> [Word64]
toWord64 bytes = go (map fromIntegral bytes)
  where
    go (w1:w2:w3:w4:w5:w6:w7:w8:rest) =
      (w1 `shiftL` 56
         .|. w2 `shiftL` 48
         .|. w3 `shiftL` 40
         .|. w4 `shiftL` 32
         .|. w5 `shiftL` 24
         .|. w6 `shiftL` 16
         .|. w7 `shiftL` 8
         .|. w8)
        : go rest
    go [] = []
    go _ = unreachable

fromWord64 :: [Word64] -> [Word8]
fromWord64 = concatMap f
  where
    f word =
      map
        fromIntegral
        [ word `shiftR` 56
        , word `shiftR` 48
        , word `shiftR` 40
        , word `shiftR` 32
        , word `shiftR` 24
        , word `shiftR` 16
        , word `shiftR` 8
        , word
        ]

pad101 :: Int -> Int -> [Word8]
pad101 x m =
  let pad = (x - (m `mod` x))
   in [0x80] ++ replicate (pad - 2) 0 ++ [0x1]

-- sponge :: [Word8] -> Int -> [Word64]
-- sponge n d =
--   let a = absorb (replicate 25 0) (toWord64 (n ++ pad101 72 (length n)))
--    in squeeze a d (take 72 a)
--   where
--     absorb state n' =
--       let state' =
--             zipWith xor state (keccak_f1600 (take 9 n' ++ replicate 16 0))
--        in absorb state' (drop 9 n')
--     squeeze st outl z
--       | outl <= length z = take outl z
--       | otherwise =
--         let st' = keccak_f1600 st
--          in squeeze st' outl (z ++ take 72 st')

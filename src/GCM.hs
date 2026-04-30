module GCM where

import Data.Bits
import Data.Word (Word32)
import Util (unreachable)

clshl128 :: [Word32] -> [Word32]
clshl128 [x1, x2, x3, x4] =
    let (y1, c1) = shlwcarry x1
        (y2, c2) = shlwcarry x2
        (y3, c3) = shlwcarry x3
        (y4, c4) = shlwcarry x4
     in [if c4 == 1 then y1 `xor` 0x87 else y1, y2 + c1, y3 + c2, y4 + c3]
  where
    shlwcarry z = (z `shiftL` 1, z `shiftR` 31)
clshl128 _ = unreachable

testBit128 :: [Word32] -> Int -> Bool
testBit128 xs@[_, _, _, _] i =
    let word = i `div` 32
        bi = i `mod` 32
     in ((xs !! word) `shiftR` bi) .&. 1 == 1
testBit128 _ _ = unreachable

clmul128 :: [Word32] -> [Word32] -> [Word32]
clmul128 a b = go a [0, 0, 0, 0] b 0
  where
    go _ z _ 128 = z
    go v z y i =
        let z' = if testBit128 y i then zipWith xor z v else z
         in go (clshl128 v) z' y (i + 1)

ghash :: [Word32] -> [Word32] -> [Word32]
ghash input hashkey
    | length input `mod` 4 /= 0 = unreachable
    | otherwise = go input [0, 0, 0, 0] hashkey
  where
    go [] y _ = y
    go x y h = go (drop 4 x) (clmul128 (zipWith xor (take 4 x) y) h) h

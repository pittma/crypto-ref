module GF where

import Data.Bits (Bits(xor, shiftL, shiftR, (.&.)))

import Util

inv :: (Num b, Bits b) => b -> b -> b
inv _ 0 = unreachable
inv m a = go m a 0 1
  where
    go _ 1 _ t = t
    go r0 r1 s t =
      let (q, r) = blq r0 r1
          t' = s `xor` q `multGF` t
       in go r1 r t t'
  
multGF :: (Bits b, Num b) => b -> b -> b
multGF x y = go x y 0 8
  where
    go :: (Bits b, Num b) => b -> b -> b -> b -> b
    go _ _ p 0 = p
    go a b p i =
      let p' =
            if b .&. 1 == 1
              then a `xor` p
              else p
          a' =
            if a .&. 0x80 == 0x80
              then (a `shiftL` 1) `xor` 0x1b
              else a `shiftL` 1
       in go a' (b `shiftR` 1) p' (i - 1)
  
degree :: (Bits b, Num b) => b -> Int
degree 0 = 0
degree a = go a 0
  where
    go x r
      | x == 1 = r
      | otherwise = go (shiftR x 1) (r + 1)


blq :: (Bits b, Num b) => b -> b -> (b, b)
blq a b = go a b 0
  where
    go x y q
      | degree x < degree y = (q, x)
      | otherwise =
        let shift = degree x - degree y
         in go (x `xor` shiftL y shift) y (q `xor` (1 `shiftL` shift))

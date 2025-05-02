module AES.Encrypt where

import Prelude hiding (round)

import Data.Bits
import Data.Word (Word8, Word32)

import Util
import GF

import AES.Shared

shiftRows :: [Word32] -> [Word32]
shiftRows [a, b, c, d] = [a, b `rotateL` 8, c `rotateL` 16, d `rotateL` 24]
shiftRows _ = unreachable

mixColumns :: [Word32] -> [Word32]
mixColumns = columnTrans mix

mix :: [Word8] -> [Word8]
mix [a, b, c, d] =
  let a' = mca [a, b, c, d]
      b' = mcb [a, b, c, d]
      c' = mcc [a, b, c, d]
      d' = mcd [a, b, c, d]
   in [a', b', c', d']
mix _ = unreachable

mca :: [Word8] -> Word8
mca [a, b, c, d] = (0x02 `multGF` a) `xor` (0x03 `multGF` b) `xor` c `xor` d
mca _ = unreachable

mcb :: [Word8] -> Word8
mcb [a, b, c, d] = a `xor` (0x02 `multGF` b) `xor` (0x03 `multGF` c) `xor` d
mcb _ = unreachable

mcc :: [Word8] -> Word8
mcc [a, b, c, d] = a `xor` b `xor` (0x02 `multGF` c) `xor` (0x03 `multGF` d)
mcc _ = unreachable

mcd :: [Word8] -> Word8
mcd [a, b, c, d] = (0x03 `multGF` a) `xor` b `xor` c `xor` (0x02 `multGF` d)
mcd _ = unreachable

round :: [Word32] -> [Word32] -> [Word32]
round rk = addRoundKey rk . mixColumns . shiftRows . subBytes

roundLast :: [Word32] -> [Word32] -> [Word32]
roundLast rk = addRoundKey rk . shiftRows . subBytes

encrypt_ :: Int -> [Word32] -> [Word32] -> [Word32]
encrypt_ rnd plain expandedKey =
  go (addRoundKey (take 4 expandedKey) (matrify plain)) (drop 4 expandedKey) 1
  where
    go :: [Word32] -> [Word32] -> Int -> [Word32]
    go state ek i
      | i == rnd = matrify $ roundLast ek state
      | otherwise = go (round (take 4 ek) state) (drop 4 ek) (i + 1)

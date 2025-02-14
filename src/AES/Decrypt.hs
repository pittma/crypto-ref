module AES.Decrypt where

import Data.Bits
import Data.Word

import GF
import Util

import AES.Shared

invConstC :: Word8
invConstC = 0x05
  
invShiftRows :: [Word32] -> [Word32]
invShiftRows [a, b, c, d] = [a, b `rotateR` 8, c `rotateR` 16, d `rotateR` 24]
invShiftRows _ = unreachable

invAffine :: Word8 -> Word8
invAffine w =
  (w `rotateL` 1) `xor` (w `rotateL` 3) `xor` (w `rotateL` 6) `xor` invConstC

invSubByte :: Word8 -> Word8
invSubByte = inv' . invAffine

invSubWord :: Word32 -> Word32
invSubWord = sub invSubByte

invSubBytes :: [Word32] -> [Word32]
invSubBytes = map invSubWord

invMixColumns :: [Word32] -> [Word32]
invMixColumns = columnTrans mix

mix :: [Word8] -> [Word8]
mix [a, b, c, d] =
  let a' = mca [a, b, c, d]
      b' = mcb [a, b, c, d]
      c' = mcc [a, b, c, d]
      d' = mcd [a, b, c, d]
   in [a', b', c', d']
mix _ = unreachable

mca :: [Word8] -> Word8
mca [a, b, c, d] =
  (0x0e `multGF` a)
    `xor` (0x0b `multGF` b)
    `xor` (0x0d `multGF` c)
    `xor` (0x09 `multGF` d)
mca _ = unreachable

mcb :: [Word8] -> Word8
mcb [a, b, c, d] =
  (0x09 `multGF` a)
    `xor` (0x0e `multGF` b)
    `xor` (0x0b `multGF` c)
    `xor` (0x0d `multGF` d)
mcb _ = unreachable

mcc :: [Word8] -> Word8
mcc [a, b, c, d] =
  (0x0d `multGF` a)
    `xor` (0x09 `multGF` b)
    `xor` (0x0e `multGF` c)
    `xor` (0x0b `multGF` d)
mcc _ = unreachable

mcd :: [Word8] -> Word8
mcd [a, b, c, d] =
  (0x0b `multGF` a)
    `xor` (0x0d `multGF` b)
    `xor` (0x09 `multGF` c)
    `xor` (0x0e `multGF` d)
mcd _ = unreachable

invRound :: [Word32] -> [Word32] -> [Word32]
invRound kr = invMixColumns . addRoundKey kr . invSubBytes . invShiftRows

invRoundLast :: [Word32] -> [Word32] -> [Word32]
invRoundLast kr = addRoundKey kr . invSubBytes . invShiftRows

decrypt_ :: [Word32] -> [Word32] -> [Word32]
decrypt_ ciphert expandedKey =
  let ekl = length expandedKey
   in go
        (addRoundKey (drop (ekl - 4) expandedKey) (matrify ciphert))
        (take (ekl - 4) expandedKey)
        1
        (ekl - 4)
  where
    go :: [Word32] -> [Word32] -> Int -> Int -> [Word32]
    go state ek 10 _ = matrify $ invRoundLast ek state
    go state ek i ekl =
      go
        (invRound (drop (ekl - 4) ek) state)
        (take (ekl - 4) ek)
        (i + 1)
        (ekl - 4)

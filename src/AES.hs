module AES where

import Data.List (foldl')
import Data.Bits (Bits(rotateL, xor, shiftL))
import Data.Word (Word8, Word16, Word32)

import Util
import GF

-- | This AES's fixed polynomial, found in the standard, 4.2.
-- | x^8 + x^4 + x^3 + x + 1
-- | or 100011011
irrPoly :: Word16
irrPoly = 283
irrPolyDeg :: Int
irrPolyDeg = 8

-- | This it the constant c used in the affine transformation for the
-- | S-Box table, section 5.1.1.
constC :: Word8
constC = 0x63

subWord :: Word32 -> Word32
subWord w = bytesToWord $ map subByte (wordToBytes w)

subBytes :: [Word32] -> [Word32]
subBytes = map subWord

shiftRows :: [Word32] -> [Word32]
shiftRows [a, b, c, d] = [a, b `rotateL` 8, c `rotateL` 16, d `rotateL` 24]
shiftRows _ = unreachable

affine :: Word8 -> Word8
affine x =
  x
    `xor` (x `rotateL` 1)
    `xor` (x `rotateL` 2)
    `xor` (x `rotateL` 3)
    `xor` (x `rotateL` 4)
    `xor` constC

inv' :: Word8 -> Word8
inv' 0 = 0
inv' a = fromIntegral $ inv irrPoly (fromIntegral a)

subByte :: Word8 -> Word8
subByte = affine . inv'

  
mixColumns :: [Word32] -> [Word32]
mixColumns state = unmatrify . transpose $ map mix (transpose . matrify $ state)

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

rcon :: Int -> Word32
rcon i = shiftL (go i) 24
  where
    go i' = fromIntegral (foldl' f 1 [1 .. i' - 1])
    f x _ =
      let x' = x `shiftL` 1
       in if x' > 255
            then x' `xor` irrPoly
            else x'

rotWord :: Word32 -> Word32
rotWord = flip rotateL 8

expandKey :: [Word32] -> [Word32]
expandKey key
  | length key == 4 = snd (go 4 10)
  | length key == 8 = snd (go 8 14)
  | otherwise = unreachable
  where
    go nk rds = foldl' f (last key, key) [length key .. (4 * (rds + 1)) - 1]
      where
        f (prv, ek) i =
          let nxt
                | i `mod` nk == 0 =
                  subWord (rotWord prv) `xor` rcon (i `div` nk)
                | nk > 6 && i `mod` nk == 4 = subWord prv
                | otherwise = prv
              nxt' = nxt `xor` (ek !! (i - nk))
           in (nxt', ek ++ [nxt'])

ekStdExample :: [Word32]
ekStdExample = map bytesToWord e
  where
    e =
      [ [0x2b, 0x7e, 0x15, 0x16]
      , [0x28, 0xae, 0xd2, 0xa6]
      , [0xab, 0xf7, 0x15, 0x88]
      , [0x09, 0xcf, 0x4f, 0x3c]
      ]

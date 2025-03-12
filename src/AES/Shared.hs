module AES.Shared where

import Data.Bits
import Data.Word
import Data.List hiding (transpose)

import GF
import Util

-- | This AES's fixed polynomial, found in the standard, 4.2.
-- | x^8 + x^4 + x^3 + x + 1
-- | or 100011011
irrPoly :: Word16
irrPoly = 283

-- | The degree of the irreducible polynomial.
irrPolyDeg :: Int
irrPolyDeg = 8

-- | This it the constant c used in the affine transformation for the
-- | S-Box table, section 5.1.1.
constC :: Word8
constC = 0x63


inv' :: Word8 -> Word8
inv' 0 = 0
inv' a = fromIntegral $ inv irrPoly (fromIntegral a)

columnTrans :: ([Word8] -> [Word8]) -> [Word32] -> [Word32]
columnTrans mixf state =
  map bytesToWord $ transpose $ map mixf $ transpose $ map wordToBytes state

addRoundKey :: [Word32] -> [Word32] -> [Word32]
addRoundKey rk state = zipWith xor state (matrify rk)

rcon :: Int -> Word32
rcon i = shiftL (go i) 24
  where
    go i' = fromIntegral (foldl' f 1 [1 .. i' - 1])
    f x _ =
      let x' = x `shiftL` 1
       in if x' > 255
            then x' `xor` irrPoly
            else x'

-- Key expansion and encryption share the same S-Box, so encryption's
-- subBytes implementation lives here.

affine :: Word8 -> Word8
affine x =
  x
    `xor` (x `rotateL` 1)
    `xor` (x `rotateL` 2)
    `xor` (x `rotateL` 3)
    `xor` (x `rotateL` 4)
    `xor` constC

subByte :: Word8 -> Word8
subByte = affine . inv'
  
sub :: (Word8 -> Word8) -> Word32 -> Word32
sub subf w = bytesToWord $ map subf (wordToBytes w)

subWord :: Word32 -> Word32
subWord = sub subByte

subBytes :: [Word32] -> [Word32]
subBytes = map subWord

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

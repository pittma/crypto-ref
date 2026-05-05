module GCM where

import Data.Bits
import Data.Word (Word32)

import qualified AES.Encrypt as AE
import qualified AES.Shared as AS
import Util (unreachable)

clshr128 :: [Word32] -> [Word32]
clshr128 [x1, x2, x3, x4] =
    let (y1, c1) = shrwcarry x1
        (y2, c2) = shrwcarry x2
        (y3, c3) = shrwcarry x3
        (y4, c4) = shrwcarry x4
     in [if c4 == (1 `shiftL` 31) then y1 `xor` (0xe1 `shiftL` 24) else y1, carrybit y2 c1, carrybit y3 c2, carrybit y4 c3]
  where
    shrwcarry z = (z `shiftR` 1, z `shiftL` 31)
    carrybit y c = y .|. c
clshr128 _ = unreachable

testBit128 :: [Word32] -> Int -> Bool
testBit128 xs@[_, _, _, _] i =
    let word = (i `div` 32)
        bi = i `mod` 32
     in ((xs !! word) `shiftR` (31 - bi)) .&. 1 == 1
testBit128 _ _ = unreachable

clmul128 :: [Word32] -> [Word32] -> [Word32]
clmul128 a b = go a [0, 0, 0, 0] b 0
  where
    go _ z _ 128 = z
    go v z y i =
        let z' = if testBit128 y i then zipWith xor z v else z
         in go (clshr128 v) z' y (i + 1)

ghash :: [Word32] -> [Word32] -> [Word32]
ghash input hashkey
    | length input `mod` 4 /= 0 = unreachable
    | otherwise = go input [0, 0, 0, 0] hashkey
  where
    go [] y _ = y
    go x y h = go (drop 4 x) (clmul128 (zipWith xor (take 4 x) y) h) h

expandInc :: [Word32] -> Int -> [Word32]
expandInc ys i = concat $ drop 1 $ take (i + 1) $ iterate f ys
  where
    f [x1, x2, x3, x4] = [x1, x2, x3, x4 + 1]
    f _ = unreachable

encrypt_ :: Int -> [Word32] -> [Word32] -> [Word32] -> ([Word32], [Word32])
encrypt_ rnds key initVec plaintext =
    let ekey = AS.expandKey key
        h = AE.encrypt_ rnds [0, 0, 0, 0] ekey
        y0 = initVec ++ [1]
        ptl = length plaintext
        wbl = ptl `div` 4
        tl = ptl `mod` 4
        ys =
            if tl == 0
                then expandInc y0 wbl
                else expandInc y0 (wbl + 1)
        c = go ekey (take (4 * wbl) plaintext) ys
        cstar =
            if tl == 0
                then []
                else
                    zipWith
                        xor
                        (drop (wbl * 4) plaintext)
                        (take tl (AE.encrypt_ rnds (drop (wbl * 4) ys) ekey))
        ccomp = c ++ cstar
        clen = (length ccomp) * 4 * 8
        tmpclenvecTODO = [0, 0, 0, fromIntegral clen]
     in (ccomp, zipWith xor (ghash (c ++ tmpclenvecTODO) h) (AE.encrypt_ rnds y0 ekey))
  where
    go _ [] _ = []
    go k pt ys =
        zipWith
            xor
            (take 4 pt)
            (AE.encrypt_ rnds (take 4 ys) k)
            ++ go k (drop 4 pt) (drop 4 ys)

encrypt128 :: [Word32] -> [Word32] -> [Word32] -> ([Word32], [Word32])
encrypt128 = encrypt_ 10

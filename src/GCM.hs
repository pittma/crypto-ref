module GCM where

import Data.Bits
import Data.Word (Word32, Word64)

import qualified AES.Encrypt as AE
import qualified AES.Shared as AS
import Util (unreachable)

clshr128 :: [Word32] -> [Word32]
clshr128 [x1, x2, x3, x4] =
    let (y1, c1) = shrwcarry x1
        (y2, c2) = shrwcarry x2
        (y3, c3) = shrwcarry x3
        (y4, c4) = shrwcarry x4
     in [ if c4 == (1 `shiftL` 31) then y1 `xor` (0xe1 `shiftL` 24) else y1
        , carrybit y2 c1
        , carrybit y3 c2
        , carrybit y4 c3
        ]
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
expandInc iv len =
    -- +2 or +1 here because we're always going to be +1 after dropping the
    -- initial value, +2 accounts for the incomplete tail block.
    let cnt = if len `mod` 4 /= 0 then (len `div` 4) + 2 else len + 1
     in concat $ drop 1 $ take cnt $ iterate f iv
  where
    f [x1, x2, x3, x4] = [x1, x2, x3, x4 + 1]
    f _ = unreachable

splitQWord :: Word64 -> [Word32]
splitQWord x = [fromIntegral (x `shiftR` 32), fromIntegral x]

padToBlock :: [Word32] -> [Word32]
padToBlock x =
    let xlm = length x `mod` 4
     in if xlm /= 0
            then
                x ++ replicate (4 - xlm) 0
            else x

handleWholeBlocks ::
    ([Word32] -> [Word32] -> [Word32]) ->
    [Word32] ->
    [Word32] ->
    [Word32] ->
    [Word32]
handleWholeBlocks _ _ [] _ = []
handleWholeBlocks f k d ys =
    zipWith
        xor
        (take 4 d)
        (f (take 4 ys) k)
        ++ handleWholeBlocks f k (drop 4 d) (drop 4 ys)

handleTail ::
    ([Word32] -> [Word32] -> [Word32]) ->
    Int ->
    [Word32] ->
    [Word32] ->
    [Word32] ->
    [Word32]
handleTail f tl d y k = zipWith xor d (take tl (f y k))

transform :: ([Word32] -> [Word32] -> [Word32]) -> [Word32] -> [Word32] -> [Word32] -> [Word32]
transform f k d iv =
    let dataLen = length d
        tl = dataLen `mod` 4
        wbl = dataLen `div` 4
        ys = expandInc iv dataLen
        t = drop (wbl * 4) d
        y = drop (wbl * 4) ys
     in handleWholeBlocks f k (take (wbl * 4) d) ys ++ handleTail f tl t y k

mkLenVec :: [Word32] -> [Word32]
mkLenVec x = splitQWord $ fromIntegral $ length x * 4 * 8

encrypt_ :: Int -> [Word32] -> [Word32] -> [Word32] -> [Word32] -> ([Word32], [Word32])
encrypt_ rnds key initVec plaintext aad =
    let ekey = AS.expandKey key
        h = AE.encrypt_ rnds [0, 0, 0, 0] ekey
        y0 = initVec ++ [1]
        c = transform (AE.encrypt_ rnds) ekey plaintext y0
     in ( c
        , zipWith
            xor
            (ghash (padToBlock aad ++ padToBlock c ++ mkLenVec aad ++ mkLenVec c) h)
            (AE.encrypt_ rnds y0 ekey)
        )

encrypt128 :: [Word32] -> [Word32] -> [Word32] -> [Word32] -> ([Word32], [Word32])
encrypt128 = encrypt_ 10

decrypt_ :: Int -> [Word32] -> [Word32] -> [Word32] -> [Word32] -> [Word32] -> Maybe [Word32]
decrypt_ rnds key initVec ct tag aad =
    let ekey = AS.expandKey key
        h = AE.encrypt_ rnds [0, 0, 0, 0] ekey
        y0 = initVec ++ [1]
        t =
            zipWith
                xor
                (ghash (padToBlock aad ++ padToBlock ct ++ mkLenVec aad ++ mkLenVec ct) h)
                (AE.encrypt_ rnds y0 ekey)
     in if t /= tag
            then
                Nothing
            else
                Just $ transform (AE.encrypt_ rnds) ekey ct y0

decrypt128 :: [Word32] -> [Word32] -> [Word32] -> [Word32] -> [Word32] -> Maybe [Word32]
decrypt128 = decrypt_ 10

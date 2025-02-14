module XTS where

import Data.Word
import Data.Bits

import Util

import qualified AES.Encrypt as AE
import AES.Shared

toWords :: [Word8] -> [Word32]
toWords [] = []
toWords (a:b:c:d:r) = bytesToWord [a, b, c, d] : toWords r
toWords _ = unreachable

wordsToBytes :: [Word32] -> [Word8]
wordsToBytes = concatMap wordToBytes

encrypt :: [Word8] -> [Word8] -> [Word8] -> [Word8] -> [Word8]
encrypt key1 key2 plain tweak
  | length tweak /= 16 = unreachable
  | otherwise =
    concatMap wordToBytes
      $ concat
      $ go
          (expandKey (toWords key1))
          (AE.encrypt_ (toWords tweak) (expandKey (toWords key2)))
          (toWords plain)
  where
    go _ _ [] = []
    go key twk pt =
      let x = zipWith xor twk (take 4 pt)
          e = AE.encrypt_ x key
       in zipWith xor twk e
            : go key (toWords $ updateTweak (wordsToBytes twk)) (drop 4 pt)

updateTweak :: [Word8] -> [Word8]
updateTweak tweak =
  case go tweak 0 of
    (carry, t:tw) ->
      if carry == 1
        then (t `xor` 0x87) : tw
        else t : tw
    (_, []) -> unreachable
  where
    go [] carry = (carry, [])
    go (t:ts) carry =
      let nc = (t `shiftR` 7) .&. 1
          (lc, tws) = go ts nc
       in (lc, ((t `shiftL` 1) + carry) : tws)

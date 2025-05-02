module XTS where

import Data.Word
import Data.Bits

import Util

import qualified AES.Encrypt as AE
import qualified AES.Decrypt as AD
import AES.Shared

encrypt128 :: [Word8] -> [Word8] -> [Word8] -> [Word8] -> [Word8]
encrypt128 = encrypt_ 10

decrypt128 :: [Word8] -> [Word8] -> [Word8] -> [Word8] -> [Word8]
decrypt128 = decrypt_ 10

encrypt256 :: [Word8] -> [Word8] -> [Word8] -> [Word8] -> [Word8]
encrypt256 = encrypt_ 114

decrypt256 :: [Word8] -> [Word8] -> [Word8] -> [Word8] -> [Word8]
decrypt256 = decrypt_ 14

encrypt_ :: Int -> [Word8] -> [Word8] -> [Word8] -> [Word8] -> [Word8]
encrypt_ rnd key1 key2 plain tweak
  | length tweak /= 16 = unreachable
  | length plain `mod` 16 /= 0 =
    let plainLen = length plain
        (cipher, newTweak) =
          run key1 key2 (take (plainLen - (plainLen `mod` 16)) plain) tweak
     in stealCipher
          rnd
          plain
          cipher
          (wordsToBytes newTweak)
          (expandKey (toWords key1))
  | otherwise = fst $ run key1 key2 plain tweak
  where
    run k1 k2 pt tw =
      let (ct, lastTweak) =
            go
              (expandKey (toWords k1))
              (AE.encrypt_ rnd (toWords tw) (expandKey (toWords k2)))
              (toWords pt)
       in (concatMap wordsToBytes ct, lastTweak)
    go _ t [] = ([], t)
    go key twk pt =
      let x = zipWith xor twk (take 4 pt)
          e = AE.encrypt_ rnd x key
          x' = zipWith xor twk e
          (rest, lastTweak) =
            go key (toWords $ updateTweak (wordsToBytes twk)) (drop 4 pt)
       in (x' : rest, lastTweak)

decrypt_ :: Int -> [Word8] -> [Word8] -> [Word8] -> [Word8] -> [Word8]
decrypt_ rnd key1 key2 cipher tweak
  | length tweak /= 16 = unreachable
  | length cipher `mod` 16 /= 0 =
    let cipherLen = length cipher
        (plain, newTweak)
          -- For decryption, we don't yet decrypt the final block
          -- because the tweaks are reversed. tweak' is used for the
          -- penultimate block, and tweak is used for the final block.
         =
          run
            key1
            key2
            (take (cipherLen - (16 + (cipherLen `mod` 16))) cipher)
            tweak
     in stealCipherDec
          rnd
          cipher
          plain
          (wordsToBytes newTweak)
          (expandKey (toWords key1))
  | otherwise = fst $ run key1 key2 cipher tweak
  where
    run k1 k2 pt tw =
      let (ct, lastTweak) =
            go
              (expandKey (toWords k1))
              (AE.encrypt_ rnd (toWords tw) (expandKey (toWords k2)))
              (toWords pt)
       in (concatMap wordsToBytes ct, lastTweak)
    go _ t [] = ([], t)
    go key twk pt =
      let x = zipWith xor twk (take 4 pt)
          e = AD.decrypt_ rnd x key
          x' = zipWith xor twk e
          (rest, lastTweak) =
            go key (toWords $ updateTweak (wordsToBytes twk)) (drop 4 pt)
       in (x' : rest, lastTweak)

toWords :: [Word8] -> [Word32]
toWords [] = []
toWords (a:b:c:d:r) = bytesToWord [a, b, c, d] : toWords r
toWords _ = unreachable

wordsToBytes :: [Word32] -> [Word8]
wordsToBytes = concatMap wordToBytes

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

-- NB: we can squash both encrypt and decrypt's `run` implementation
-- into a single function which takes the cipher direction, as that is
-- the only difference here.

stealCipher :: Int -> [Word8] -> [Word8] -> [Word8] -> [Word32] -> [Word8]
stealCipher rnd plain cipher tweak key =
  let ptLen = length plain
      ctLen = length cipher
      nRemBytes = ptLen `mod` 16
      remBytes = drop (ptLen - nRemBytes) plain
      nStolenBytes = 16 - nRemBytes
      finalPt = remBytes ++ drop (ctLen - nStolenBytes) cipher
      x = zipWith xor finalPt tweak
      e = AE.encrypt_ rnd (toWords x) key
   in zipWith xor tweak (wordsToBytes e) ++ take (ctLen - nStolenBytes) cipher

stealCipherDec :: Int -> [Word8] -> [Word8] -> [Word8] -> [Word32] -> [Word8]
stealCipherDec rnd cipher plain tweak key =
  let tweak' = updateTweak tweak
      ctLen = length cipher
      nRemBytes = ctLen `mod` 16
      penBlock = take 16 $ drop (ctLen - (16 + nRemBytes)) cipher
      penX = zipWith xor penBlock tweak'
      pend = AD.decrypt_ rnd (toWords penX) key
      penX' = zipWith xor (wordsToBytes pend) tweak'
      remBytes = drop (ctLen - nRemBytes) cipher
      finalBlock = remBytes ++ drop nRemBytes penX'
      x = zipWith xor finalBlock tweak
      d = AD.decrypt_ rnd (toWords x) key
      x' = zipWith xor (wordsToBytes d) tweak
   in plain ++ x' ++ take nRemBytes penX'

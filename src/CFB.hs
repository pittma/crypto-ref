{- CFB is dumb -}
module CFB where

import Data.Word
import Data.Bits

import qualified AES.Encrypt as E
import AES.Shared

encrypt128 :: [Word32] -> [Word32] -> [Word32] -> [Word32]
encrypt128 = encrypt_ 10

encrypt256 :: [Word32] -> [Word32] -> [Word32] -> [Word32]
encrypt256 = encrypt_ 14
  
encrypt_ :: Int -> [Word32] -> [Word32] -> [Word32] -> [Word32]
encrypt_ rnd plaintext iv key =
  let ekeys = expandKey key
      pb = zipWith xor (E.encrypt_ rnd iv ekeys) plaintext
   in go (drop 4 plaintext) pb ekeys
  where
    go [] pb _ = pb
    go pt pb keys
      | (length pt) < 4 =
        let pbe = E.encrypt_ rnd pb keys
         in pb ++ go (drop 4 pt) (zipWith xor (take (length pt) pbe) pt) keys
      | otherwise =
        let pbe = E.encrypt_ rnd pb keys
         in pb ++ go (drop 4 pt) (zipWith xor pbe pt) keys

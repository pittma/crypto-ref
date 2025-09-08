module Kyber where

import Data.Functor ((<&>))
import Data.Word

import Crypto.Random (getRandomBytes)
import qualified Data.ByteArray as BA

import Keccak (shake128, absorb, squeeze')
import Util (unreachable)

{- An implementation of ML-KEM-768.

 This involves the following parameters:

 | n   | q    | k | eta1 | eta2 | du | dv |
 |-----|------|---|------|------|----|----|
 | 256 | 3329 | 3 | 2    | 2    | 10 | 4  |
-}

{-------------------------------- CONSTANTS ----------------------------------}

n :: Int
n = 256

q :: Int
q = 3329

k :: Word8
k = 3

eta1 :: Int
eta1 = 2

eta2 :: Int
eta2 = 2

du :: Int
du = 10

dv :: Int
dv = 4

{---------------------------------- KEYGEN ------------------------------------}

keyGenIO :: IO [Word16]
keyGenIO = (getRandomBytes 32 :: IO BA.Bytes) <&> keyGen . BA.unpack
  
sampleNTT :: [Word8] -> [[Word16]]
sampleNTT = unreachable
  
keyGen :: [Word8] -> [Word16]
keyGen seed =
  let (rho, sigma) = splitAt 32 $ shake128 (seed ++ [k]) 64
   in []

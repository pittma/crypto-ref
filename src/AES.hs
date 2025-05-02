module AES where

import Data.Word

import Util

import AES.Shared
import AES.Encrypt
import AES.Decrypt

encrypt128 :: String -> String -> String
encrypt128 = run (encrypt_ 10)

decrypt128 :: String -> String -> String
decrypt128 = run (decrypt_ 10)

encrypt256 :: String -> String -> String
encrypt256 = run (encrypt_ 14)

decrypt256 :: String -> String -> String
decrypt256 = run (decrypt_ 14)

run :: ([Word32] -> [Word32] -> [Word32]) -> String -> String -> String
run f text key =
  toString $ concatMap wordToBytes (f (wordify text) (expandKey (wordify key)))

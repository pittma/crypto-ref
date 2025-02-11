module AES where

import Data.Word

import Util

import AES.Shared
import AES.Encrypt
import AES.Decrypt

encrypt :: String -> String -> String
encrypt = run encrypt_

decrypt :: String -> String -> String
decrypt = run decrypt_

run :: ([Word32] -> [Word32] -> [Word32]) -> String -> String -> String
run f text key =
  toString $ concatMap wordToBytes (f (wordify text) (expandKey (wordify key)))

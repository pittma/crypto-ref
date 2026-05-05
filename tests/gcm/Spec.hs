module Main where

import Control.Monad
import Test.Hspec

import Util

import GCM

data EncryptCase = ECase
    { pt :: String
    , iv :: String
    , key :: String
    , ct :: String
    , tag :: String
    }

encryptCases :: [EncryptCase]
encryptCases =
    [ ECase
        { pt = []
        , iv = "000000000000000000000000"
        , key = "00000000000000000000000000000000"
        , ct = []
        , tag = "58e2fccefa7e3061367f1d57a4e7455a"
        }
    , ECase
        { pt = "00000000000000000000000000000000"
        , iv = "000000000000000000000000"
        , key = "00000000000000000000000000000000"
        , ct = "0388dace60b6a392f328c2b971b2fe78"
        , tag = "ab6e47d42cec13bdf53a67b21257bddf"
        }
    ]

main :: IO ()
main =
    hspec $ do
        describe "GCM encrypt" $ do
            it "should correctly encrypt the test vectors from the standard" $ do
                forM_ encryptCases encTest
  where
    encTest :: EncryptCase -> IO ()
    encTest c = do
        let out = encrypt128 (wordify (key c)) (wordify (iv c)) (wordify (pt c))
        stringify (fst out) `shouldBe` (ct c)
        stringify (snd out) `shouldBe` (tag c)

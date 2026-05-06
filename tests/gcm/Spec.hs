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
    , aad :: String
    }

encryptCases :: [EncryptCase]
encryptCases =
    [ ECase
        { pt = []
        , iv = "000000000000000000000000"
        , key = "00000000000000000000000000000000"
        , ct = []
        , tag = "58e2fccefa7e3061367f1d57a4e7455a"
        , aad = []
        }
    , ECase
        { pt = "00000000000000000000000000000000"
        , iv = "000000000000000000000000"
        , key = "00000000000000000000000000000000"
        , ct = "0388dace60b6a392f328c2b971b2fe78"
        , tag = "ab6e47d42cec13bdf53a67b21257bddf"
        , aad = []
        }
    , ECase
        { pt = "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b391aafd255"
        , iv = "cafebabefacedbaddecaf888"
        , key = "feffe9928665731c6d6a8f9467308308"
        , ct = "42831ec2217774244b7221b784d0d49ce3aa212f2c02a4e035c17e2329aca12e21d514b25466931c7d8f6a5aac84aa051ba30b396a0aac973d58e091473f5985"
        , tag = "4d5c2af327cd64a62cf35abd2ba6fab4"
        , aad = []
        }
    , ECase
        { pt = "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39"
        , iv = "cafebabefacedbaddecaf888"
        , key = "feffe9928665731c6d6a8f9467308308"
        , ct = "42831ec2217774244b7221b784d0d49ce3aa212f2c02a4e035c17e2329aca12e21d514b25466931c7d8f6a5aac84aa051ba30b396a0aac973d58e091"
        , tag = "5bc94fbc3221a5db94fae95ae7121a47"
        , aad = "feedfacedeadbeeffeedfacedeadbeefabaddad2"
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
        let out = encrypt128 (wordify (key c)) (wordify (iv c)) (wordify (pt c)) (wordify (aad c))
        stringify (fst out) `shouldBe` (ct c)
        stringify (snd out) `shouldBe` (tag c)

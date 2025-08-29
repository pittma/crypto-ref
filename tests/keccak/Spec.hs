module Main where

import Data.Word

import Keccak

import Test.Hspec

theKnownRCs :: [Word64]
theKnownRCs =
  [ 0x0000000000000001
  , 0x0000000000008082
  , 0x800000000000808a
  , 0x8000000080008000
  , 0x000000000000808b
  , 0x0000000080000001
  , 0x8000000080008081
  , 0x8000000000008009
  , 0x000000000000008a
  , 0x0000000000000088
  , 0x0000000080008009
  , 0x000000008000000a
  , 0x000000008000808b
  , 0x800000000000008b
  , 0x8000000000008089
  , 0x8000000000008003
  , 0x8000000000008002
  , 0x8000000000000080
  , 0x000000000000800a
  , 0x800000008000000a
  , 0x8000000080008081
  , 0x8000000000008080
  , 0x0000000080000001
  , 0x8000000080008008
  ]

theZeroVectorHash :: [Word64]
theZeroVectorHash =
  [ 0xf1258f7940e1dde7
  , 0x84d5ccf933c0478a
  , 0xd598261ea65aa9ee
  , 0xbd1547306f80494d
  , 0x8b284e056253d057
  , 0xff97a42d7f8e6fd4
  , 0x90fee5a0a44647c4
  , 0x8c5bda0cd6192e76
  , 0xad30a6f71b19059c
  , 0x30935ab7d08ffc64
  , 0xeb5aa93f2317d635
  , 0xa9a6e6260d712103
  , 0x81a57c16dbcf555f
  , 0x43b831cd0347c826
  , 0x1f22f1a11a5569f
  , 0x5e5635a21d9ae61
  , 0x64befef28cc970f2
  , 0x613670957bc46611
  , 0xb87c5a554fd00ecb
  , 0x8c3ee88a1ccf32c8
  , 0x940c7922ae3a2614
  , 0x1841f924a2c509e4
  , 0x16f53526e70465c2
  , 0x75f644e97f30a13b
  , 0xeaf1ff7b5ceca249
  ]

sha512EmptyMsgHash :: String
sha512EmptyMsgHash =
  "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26"

main :: IO ()
main =
  hspec $ do
    describe "the Round Constant generator" $ do
      it "should generate the correct round constants" $ do
        map (\x -> mkRC_ x 0) [0 .. 23] `shouldBe` theKnownRCs
    describe "the keccakP implementation" $ do
      it "should generate the correct hash for the zero vector" $ do
        keccakP (replicate 25 0) `shouldBe` theZeroVectorHash
    describe "SHA3-512" $ do
      it "should return the empty message hash for the empty message" $ do
        sha3_512 [] `shouldBe` sha512EmptyMsgHash

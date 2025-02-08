{-# LANGUAGE RankNTypes #-}
module Util where

import GHC.Stack.Types (HasCallStack)
import Data.Bits ( Bits((.|.), shiftL, shiftR) )
import Text.Printf (PrintfArg, printf)
import Data.Word ( Word8, Word32 )
import Debug.Trace

unreachable :: forall a. HasCallStack  =>  a
unreachable = error "unreachable"

bytesToWord :: [Word8] -> Word32
bytesToWord x
  | length x == 4 = f (map fromIntegral x)
  | otherwise = 0
  where
    f [a, b, c, d] =
      (a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d
    f _ = unreachable

wordToBytes :: Word32 -> [Word8]
wordToBytes x = map fromIntegral [x `shiftR` 24, x `shiftR` 16, x `shiftR` 8, x]

matrify :: [Word32] -> [Word32]
matrify x = map bytesToWord $ transpose $ map wordToBytes x

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose ((x:xs):xss) =
  (x : [hds | (hds:_) <- xss]) : transpose (xs : [tls | (_:tls) <- xss])

printHex :: (PrintfArg p) => p -> String
printHex = printf "0x%08x"

traceState :: [Word32] -> [Word32]
traceState state = trace (show $ map printHex state) state

toBytes :: String -> [Word8]
toBytes (a:b:r) = read ("0x" ++ [a, b]) : toBytes r
toBytes [] = []
toBytes [_] = unreachable

toString :: [Word8] -> String
toString = concatMap (printf "%02x")

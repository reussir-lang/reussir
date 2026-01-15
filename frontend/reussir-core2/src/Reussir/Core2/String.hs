{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core2.String (
    StringToken,
    StringUniqifier,
    allocateStrToken,
    getAllStrings,
    mangleStringToToken,
)
where

import Data.Bits ((.|.))
import Data.Digest.XXHash.FFI (XXH3 (XXH3))
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable (hashWithSalt))
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Data.Word (Word64)
import Effectful (Eff, IOE, liftIO, (:>))
import Reussir.Core2.Types.String (StringToken (StringToken), StringUniqifier (..))

data Int256
    = Int256
        {-# UNPACK #-} !Word64
        {-# UNPACK #-} !Word64
        {-# UNPACK #-} !Word64
        {-# UNPACK #-} !Word64

divMod62Word :: Word64 -> Word64 -> (Word64, Word64)
divMod62Word carry w =
    let
        -- 2^64 `div` 62 = 297528130221121800
        -- 2^64 `mod` 62 = 16
        qConst = 297528130221121800
        rConst = 16

        qPart1 = carry * qConst
        rPart1 = carry * rConst

        qPart2 = w `quot` 62
        rPart2 = w `rem` 62

        rSum = rPart1 + rPart2
        (qAdj, rFinal) =
            if rSum >= 62
                then (rSum `quot` 62, rSum `rem` 62)
                else (0, rSum)
     in
        (qPart1 + qPart2 + qAdj, rFinal)

divMod62 :: Int256 -> (Int256, Int)
divMod62 (Int256 a b c d) =
    let
        (q1, r1) = divMod62Word 0 a
        (q2, r2) = divMod62Word r1 b
        (q3, r3) = divMod62Word r2 c
        (q4, r4) = divMod62Word r3 d
     in
        (Int256 q1 q2 q3 q4, fromIntegral r4)

isZero :: Int256 -> Bool
isZero (Int256 a b c d) = a .|. b .|. c .|. d == 0

digitToChar :: Int -> (Char, Bool)
digitToChar d
    | d < 10 = (toEnum (fromEnum '0' + d), True)
    | d < 36 = (toEnum (fromEnum 'a' + d - 10), False)
    | otherwise = (toEnum (fromEnum 'A' + d - 36), False)

encodeB62 :: Int256 -> (TB.Builder, Int, Bool)
encodeB62 n0 = go n0 mempty 0
  where
    go :: Int256 -> TB.Builder -> Int -> (TB.Builder, Int, Bool)
    go n acc len =
        let (q, r) = divMod62 n
            (c, isNum) = digitToChar r
            acc' = TB.fromChar c <> acc
            len' = len + 1
         in if isZero q
                then (acc', len', isNum)
                else go q acc' len'

newInt256 :: (Word64, Word64, Word64, Word64) -> Int256
newInt256 (a, b, c, d) = Int256 a b c d

strSalt0 :: Int
strSalt0 = fromIntegral (0xce021d562d083a5e :: Word64)
strSalt1 :: Int
strSalt1 = fromIntegral (0x612641935c33db21 :: Word64)
strSalt2 :: Int
strSalt2 = fromIntegral (0x5db731d63c1b4a28 :: Word64)
strSalt3 :: Int
strSalt3 = fromIntegral (0x969b6912f32bd45e :: Word64)

allocateStrToken :: (IOE :> es) => T.Text -> StringUniqifier -> Eff es StringToken
allocateStrToken str (StringUniqifier table) = do
    let xxh3Str = XXH3 str
    bucket <- liftIO $ H.lookup table xxh3Str
    case bucket of
        Just token -> return token
        Nothing -> do
            let slotOne = fromIntegral $ hashWithSalt strSalt0 xxh3Str
            let slotTwo = fromIntegral $ hashWithSalt strSalt1 xxh3Str
            let slotThree = fromIntegral $ hashWithSalt strSalt2 xxh3Str
            let slotFour = fromIntegral $ hashWithSalt strSalt3 xxh3Str
            let token = StringToken (slotOne, slotTwo, slotThree, slotFour)
            liftIO $ H.insert table xxh3Str token
            return token

getAllStrings :: (IOE :> es) => StringUniqifier -> Eff es [(T.Text, StringToken)]
getAllStrings (StringUniqifier table) = do
    kvPairs <- liftIO $ H.toList table
    return $ map (\(XXH3 str, token) -> (str, token)) kvPairs

mangleStringToToken :: StringToken -> T.Text
mangleStringToToken (StringToken (w1, w2, w3, w4)) =
    let (builder, count, isNum) = encodeB62 $ newInt256 (w1, w2, w3, w4)
        sep = if isNum then "_" else ""
     in TB.runBuilder $ "_RNvC22REUSSIR_STRING_LITERAL" <> TB.fromDec count <> sep <> builder

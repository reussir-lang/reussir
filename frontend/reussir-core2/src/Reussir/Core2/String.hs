{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core2.String (
    StringToken,
    StringUniqifier,
    allocateStrToken,
    getAllStrings,
    mangleStringToToken,
)
where

import Data.Bits (shiftL, (.|.))
import Data.Digest.XXHash.FFI (XXH3 (XXH3))
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable (hashWithSalt))
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Data.Word (Word64)
import Effectful (Eff, IOE, liftIO, (:>))
import Reussir.Core2.Types.String (StringToken (StringToken), StringUniqifier (..))

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
    let (builder, count, isNum) = encodeB62 val
        sep = if isNum then "_" else ""
     in TB.runBuilder $ "_RNvC22REUSSIR_STRING_LITERAL" <> TB.fromDec count <> sep <> builder
  where
    val :: Integer
    val =
        (toInteger w1 `shiftL` 192)
            .|. (toInteger w2 `shiftL` 128)
            .|. (toInteger w3 `shiftL` 64)
            .|. (toInteger w4)

    encodeB62 :: Integer -> (TB.Builder, Int, Bool)
    encodeB62 n
        | n < 62 =
            let (char, isNum) = digitToChar (fromIntegral n)
             in (TB.fromChar char, 1, isNum)
        | otherwise =
            let (builder, count, isNum) = encodeB62 (n `div` 62)
                (char, _) = digitToChar (fromIntegral (n `mod` 62))
             in (builder <> TB.fromChar char, count + 1, isNum)

    digitToChar :: Int -> (Char, Bool)
    digitToChar d
        | d < 10 = (toEnum (fromEnum '0' + d), True)
        | d < 36 = (toEnum (fromEnum 'a' + d - 10), False)
        | otherwise = (toEnum (fromEnum 'A' + d - 36), False)

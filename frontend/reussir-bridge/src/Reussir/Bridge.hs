{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Reussir.Bridge (
    -- * Output Target
    OutputTarget (..),
    outputTargetToC,

    -- * Optimization Options
    OptOption (..),
    optOptionToC,

    -- * Log Level
    LogLevel (..),
    logLevelToC,

    -- * Code Model
    CodeModel (..),
    codeModelToC,

    -- * Relocation Model
    RelocationModel (..),
    relocationModelToC,

    -- * Program
    Program (..),

    -- * Compilation
    compileForNativeMachine,
    compileForTarget,
    compileProgram,
    hasTPDE,
    getNativeTargetTriple,
    getNativeTargetCPU,
    getNativeTargetFeatures,

    -- * JIT Engine
    module Reussir.Bridge.JITEngine,

    -- * Logging
    module Reussir.Bridge.Logging,

    -- * String Hash
    StringHash (..),
    hashBytes,
)
where

import Data.Word (Word64, Word8)
import Foreign.C (CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (..))

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS

import Reussir.Bridge.Compiler
import Reussir.Bridge.JITEngine
import Reussir.Bridge.Logging
import Reussir.Bridge.Types

data StringHash = StringHash
    { a :: {-# UNPACK #-} !Word64
    , b :: {-# UNPACK #-} !Word64
    , c :: {-# UNPACK #-} !Word64
    , d :: {-# UNPACK #-} !Word64
    }
    deriving (Eq, Ord, Show)

instance Storable StringHash where
    sizeOf _ = 32
    alignment _ = 8
    peek ptr = do
        let bytePtr = castPtr ptr
        a <- peekElemOff bytePtr 0
        b <- peekElemOff bytePtr 8
        c <- peekElemOff bytePtr 16
        d <- peekElemOff bytePtr 24
        pure StringHash{a, b, c, d}
    poke ptr (StringHash{a, b, c, d}) = do
        let bytePtr = castPtr ptr
        pokeElemOff bytePtr 0 a
        pokeElemOff bytePtr 8 b
        pokeElemOff bytePtr 16 c
        pokeElemOff bytePtr 24 d

foreign import capi "Reussir/Bridge.h reussir_bridge_hash_bytes"
    c_reussir_bridge_hash_bytes :: Ptr Word8 -> CSize -> Ptr StringHash -> IO ()

hashBytes :: BS.ByteString -> IO StringHash
hashBytes bytes =
    BS.unsafeUseAsCStringLen bytes $ \(ptr, len) -> do
        alloca $ \out -> do
            let ptr' = castPtr ptr
            let len' = fromIntegral len
            c_reussir_bridge_hash_bytes ptr' len' out
            peek out

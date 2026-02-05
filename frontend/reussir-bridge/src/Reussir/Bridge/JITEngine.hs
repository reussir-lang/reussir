{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Reussir.Bridge.JITEngine (
    -- * Opaque Types
    ReussirJIT,

    -- * Symbol Flags
    SymbolFlag (..),
    fromSymbolFlag,
    symbolFlagNone,
    symbolFlagHasError,
    symbolFlagWeak,
    symbolFlagCommon,
    symbolFlagAbsolute,
    symbolFlagExported,
    symbolFlagCallable,
    symbolFlagMaterializationSideEffectsOnly,

    -- * Flag Operations
    (.|.),
    hasFlag,
    setFlag,
    clearFlag,
    toggleFlag,

    -- * Re-exports from Types
    OptOption (..),

    -- * JIT Engine
    withJIT,
    addLazyModule,
    addModule,
    lookupSymbol,
)
where

import Data.Bits (complement, shiftL, xor, (.&.))
import Data.ByteString (ByteString)
import Foreign (StablePtr, freeStablePtr)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal (copyBytes)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr
import Foreign.StablePtr (castStablePtrToPtr, deRefStablePtr, newStablePtr)
import Foreign.Storable (pokeByteOff)

import Data.Bits qualified as Bits
import Data.ByteString qualified as BS

import Reussir.Bridge.Types (
    LogLevel (..),
    OptOption (..),
    logLevelToC,
    optOptionToC,
 )

--------------------------------------------------------------------------------
-- Opaque Types
--------------------------------------------------------------------------------

-- | Opaque handle for the JIT engine
newtype ReussirJIT a = ReussirJIT (Ptr ())

--------------------------------------------------------------------------------
-- Callback Types
--------------------------------------------------------------------------------

{- | A callback function that returns the MLIR IR of the AST.
This callback takes ownership of the ASTStablePtr.
Returns a pointer to a C string containing MLIR IR.
-}
type ASTCallback a = a -> IO ByteString

--------------------------------------------------------------------------------
-- C API
--------------------------------------------------------------------------------

type RawPtr = Ptr ()

-- | Create a new JIT engine.
foreign import capi "Reussir/Bridge.h reussir_bridge_jit_create"
    c_reussir_bridge_jit_create ::
        -- | ast_callback_fn
        FunPtr (CallbackFn a) ->
        -- | ast_free_fn
        FunPtr (FreeFn a) ->
        -- | opt (ReussirOptOption)
        CInt ->
        -- | level (ReussirLogLevel)
        CInt ->
        IO RawPtr

-- | Destroy the JIT engine.
foreign import capi "Reussir/Bridge.h reussir_bridge_jit_destroy"
    c_reussir_bridge_jit_destroy ::
        -- | jit
        RawPtr ->
        IO ()

-- | Add a module that should be loaded lazily.
foreign import capi "Reussir/Bridge.h reussir_bridge_jit_add_lazy_module"
    c_reussir_bridge_jit_add_lazy_module ::
        -- | jit
        RawPtr ->
        -- | ast
        RawPtr ->
        -- | symbol_names (array of strings)
        Ptr CString ->
        -- | symbol_flags (array of uint8_t)
        Ptr CUChar ->
        -- | symbol_count
        CSize ->
        IO Bool

-- | Add a module that should be loaded immediately.
foreign import capi "Reussir/Bridge.h reussir_bridge_jit_add_module"
    c_reussir_bridge_jit_add_module ::
        -- | jit
        RawPtr ->
        -- | texture (MLIR module text)
        CString ->
        IO Bool

-- | Lookup a symbol in the JIT engine.
foreign import capi "Reussir/Bridge.h reussir_bridge_jit_lookup_symbol"
    c_reussir_bridge_jit_lookup_symbol ::
        -- | jit
        RawPtr ->
        -- | symbol_name
        CString ->
        -- | mangled
        Bool ->
        IO (Ptr ())

-- | Allocate a byte buffer.
foreign import capi "Reussir/Bridge.h reussir_bridge_alloc_byte_buffer"
    c_reussir_ir_alloc_byte_buffer ::
        -- | size
        CSize ->
        IO CString

--------------------------------------------------------------------------------
-- Symbol Flags
--------------------------------------------------------------------------------

-- | Symbol flags for JIT symbols (matches LLVM's JITSymbolFlags)
newtype SymbolFlag = SymbolFlag CUChar
    deriving (Eq, Show)

-- | Extract the raw CUChar value from a SymbolFlag
fromSymbolFlag :: SymbolFlag -> CUChar
fromSymbolFlag (SymbolFlag x) = x

-- | No flags set
symbolFlagNone :: SymbolFlag
symbolFlagNone = SymbolFlag 0

-- | Symbol lookup encountered an error
symbolFlagHasError :: SymbolFlag
symbolFlagHasError = SymbolFlag (1 `shiftL` 0) -- 0x01

-- | Weak symbol (can be overridden by non-weak symbols)
symbolFlagWeak :: SymbolFlag
symbolFlagWeak = SymbolFlag (1 `shiftL` 1) -- 0x02

-- | Common symbol (weak definition, merged by linker)
symbolFlagCommon :: SymbolFlag
symbolFlagCommon = SymbolFlag (1 `shiftL` 2) -- 0x04

-- | Absolute symbol (address is absolute, not relative)
symbolFlagAbsolute :: SymbolFlag
symbolFlagAbsolute = SymbolFlag (1 `shiftL` 3) -- 0x08

-- | Exported symbol (visible to other modules)
symbolFlagExported :: SymbolFlag
symbolFlagExported = SymbolFlag (1 `shiftL` 4) -- 0x10

-- | Callable symbol (can be called as a function)
symbolFlagCallable :: SymbolFlag
symbolFlagCallable = SymbolFlag (1 `shiftL` 5) -- 0x20

-- | Materialization has side effects only (symbol value is not used)
symbolFlagMaterializationSideEffectsOnly :: SymbolFlag
symbolFlagMaterializationSideEffectsOnly = SymbolFlag (1 `shiftL` 6) -- 0x40

-- | Combine multiple symbol flags using bitwise OR
(.|.) :: SymbolFlag -> SymbolFlag -> SymbolFlag
(SymbolFlag a) .|. (SymbolFlag b) = SymbolFlag (a Bits..|. b)

infixl 5 .|.

-- | Check if a flag is set using bitwise AND
hasFlag :: SymbolFlag -> SymbolFlag -> Bool
hasFlag (SymbolFlag flags) (SymbolFlag flag) = (flags .&. flag) /= 0

-- | Set a specific flag
setFlag :: SymbolFlag -> SymbolFlag -> SymbolFlag
setFlag (SymbolFlag flags) (SymbolFlag flag) = SymbolFlag (flags Bits..|. flag)

-- | Clear a specific flag
clearFlag :: SymbolFlag -> SymbolFlag -> SymbolFlag
clearFlag (SymbolFlag flags) (SymbolFlag flag) = SymbolFlag (flags .&. complement flag)

-- | Toggle a specific flag
toggleFlag :: SymbolFlag -> SymbolFlag -> SymbolFlag
toggleFlag (SymbolFlag flags) (SymbolFlag flag) = SymbolFlag (flags `xor` flag)

--------------------------------------------------------------------------------
-- High-level API
--------------------------------------------------------------------------------

type FreeFn a = StablePtr a -> IO ()
foreign import ccall "wrapper"
    mkFreeFn :: FreeFn a -> IO (FunPtr (FreeFn a))

type CallbackFn a = StablePtr a -> IO CString
foreign import ccall "wrapper"
    mkCallbackFn :: CallbackFn a -> IO (FunPtr (CallbackFn a))

copyBSInto :: Ptr CChar -> BS.ByteString -> IO ()
copyBSInto dst bs =
    BS.useAsCStringLen bs $ \(src, len) -> do
        copyBytes dst src len
        pokeByteOff dst len (0 :: CChar)

-- | Create a new JIT engine.
withJIT ::
    ASTCallback a -> OptOption -> LogLevel -> (ReussirJIT a -> IO b) -> IO b
withJIT astCallback optOption logOption continuation = do
    freeFn <- mkFreeFn $ \stablePtr -> do
        freeStablePtr stablePtr
    callbackFn <- mkCallbackFn $ \stablePtr -> do
        node <- deRefStablePtr stablePtr
        bytes <- astCallback node
        buffer <- c_reussir_ir_alloc_byte_buffer (fromIntegral (BS.length bytes + 1))
        copyBSInto buffer bytes
        return buffer
    let optOptionC = optOptionToC optOption
    let logOptionC = logLevelToC logOption
    jit <- c_reussir_bridge_jit_create callbackFn freeFn optOptionC logOptionC
    res <- continuation (ReussirJIT jit)
    c_reussir_bridge_jit_destroy jit
    pure res

{- | Add a lazy module to the JIT engine.
The module will be materialized on demand when symbols are looked up.
The AST node is converted to a StablePtr that will be passed to the callback.
-}
addLazyModule :: ReussirJIT a -> a -> [(ByteString, SymbolFlag)] -> IO Bool
addLazyModule (ReussirJIT jit) node symbolFlags = do
    stablePtr <- newStablePtr node
    let (names, flags) = unzip symbolFlags
        flagsRaw = map fromSymbolFlag flags
        count = fromIntegral (length symbolFlags)
    -- Convert all ByteString names to CStrings
    withMany BS.useAsCString names $ \namePtrs ->
        -- Create array of CString pointers
        withArrayLen namePtrs $ \_ namePtrsArray ->
            -- Create array of flags
            withArrayLen flagsRaw $ \_ flagsArray ->
                c_reussir_bridge_jit_add_lazy_module
                    jit
                    (castStablePtrToPtr stablePtr)
                    namePtrsArray
                    flagsArray
                    count
  where
    -- Helper to apply a bracketed operation to multiple items
    withMany :: (a -> (b -> IO c) -> IO c) -> [a] -> ([b] -> IO c) -> IO c
    withMany _ [] f = f []
    withMany withF (x : xs) f = withF x $ \x' -> withMany withF xs $ \xs' -> f (x' : xs')

{- | Add a module to the JIT engine immediately.
The module text will be parsed and compiled right away.
-}
addModule :: ReussirJIT a -> ByteString -> IO Bool
addModule (ReussirJIT jit) bytes = do
    BS.useAsCString bytes $ \cString -> do
        c_reussir_bridge_jit_add_module jit cString

{- | Lookup a symbol in the JIT engine.
Returns a pointer to the symbol, or nullPtr if not found.
-}
lookupSymbol :: ReussirJIT a -> ByteString -> Bool -> IO (Ptr ())
lookupSymbol (ReussirJIT jit) symbolName mangled =
    BS.useAsCString symbolName $ \namePtr ->
        c_reussir_bridge_jit_lookup_symbol jit namePtr mangled

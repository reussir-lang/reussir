{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Reussir.Bridge
  ( -- * Output Target
    OutputTarget (..),
    outputTargetToC,

    -- * Optimization Options
    OptOption (..),
    optOptionToC,

    -- * Log Level
    LogLevel (..),
    logLevelToC,

    -- * Compilation
    compileForNativeMachine,

    -- * Target Information
    getNativeTargetTriple,
  )
where

import Foreign.C.String
import Foreign.C.Types

-------------------------------------------------------------------------------
-- C Enums
-------------------------------------------------------------------------------

-- | Output target for compilation
data OutputTarget
  = -- | LLVM IR output
    OutputLLVMIR
  | -- | Assembly output
    OutputASM
  | -- | Object file output
    OutputObject
  deriving (Eq, Show, Enum)

-- | Convert Haskell OutputTarget to C enum value
outputTargetToC :: OutputTarget -> CInt
outputTargetToC OutputLLVMIR = 0
outputTargetToC OutputASM = 1
outputTargetToC OutputObject = 2

-- | Optimization option
data OptOption
  = -- | No optimization
    OptNone
  | -- | Default optimization (O2)
    OptDefault
  | -- | Aggressive optimization (O3)
    OptAggressive
  | -- | Optimize for size (Os)
    OptSize
  | -- | TPDE optimization (if available)
    OptTPDE
  deriving (Eq, Show, Enum)

-- | Convert Haskell OptOption to C enum value
optOptionToC :: OptOption -> CInt
optOptionToC OptNone = 0
optOptionToC OptDefault = 1
optOptionToC OptAggressive = 2
optOptionToC OptSize = 3
optOptionToC OptTPDE = 4

-- | Log level
data LogLevel
  = -- | Error messages only
    LogError
  | -- | Warnings and errors
    LogWarning
  | -- | Informational messages
    LogInfo
  | -- | Debug messages
    LogDebug
  | -- | Trace messages (most verbose)
    LogTrace
  deriving (Eq, Show, Enum)

-- | Convert Haskell LogLevel to C enum value
logLevelToC :: LogLevel -> CInt
logLevelToC LogError = 0
logLevelToC LogWarning = 1
logLevelToC LogInfo = 2
logLevelToC LogDebug = 3
logLevelToC LogTrace = 4

-------------------------------------------------------------------------------
-- FFI Imports
-------------------------------------------------------------------------------

foreign import capi "Reussir/Bridge.h reussir_bridge_compile_for_native_machine"
  c_reussir_bridge_compile_for_native_machine ::
    -- | mlir_module
    CString ->
    -- | source_name
    CString ->
    -- | output_file
    CString ->
    -- | target (ReussirOutputTarget)
    CInt ->
    -- | opt (ReussirOptOption)
    CInt ->
    -- | log_level (ReussirLogLevel)
    CInt ->
    IO ()

foreign import capi "Reussir/Bridge.h reussir_bridge_alloc_native_target_triple"
  c_reussir_bridge_alloc_native_target_triple ::
    IO CString

foreign import capi "Reussir/Bridge.h reussir_bridge_free_native_target_triple"
  c_reussir_bridge_free_native_target_triple ::
    -- | triple (C string to free)
    CString ->
    IO ()

-------------------------------------------------------------------------------
-- Haskell API
-------------------------------------------------------------------------------

-- | Compile MLIR module for the native machine target
--
-- This function takes an MLIR module as a string, compiles it to the specified
-- output format with the given optimization level, and writes the result to
-- the output file.
--
-- Example:
--
-- @
-- compileForNativeMachine
--   "module {}"
--   "example.mlir"
--   "output.o"
--   OutputObject
--   OptDefault
--   LogInfo
-- @
compileForNativeMachine ::
  -- | MLIR module content (must be null-terminated or will be null-terminated)
  String ->
  -- | Source name (for diagnostics)
  String ->
  -- | Output file path
  FilePath ->
  -- | Output target format
  OutputTarget ->
  -- | Optimization level
  OptOption ->
  -- | Log level
  LogLevel ->
  IO ()
compileForNativeMachine mlirModule sourceName outputFile target opt logLevel =
  withCString mlirModule $ \mlirPtr ->
    withCString sourceName $ \sourceNamePtr ->
      withCString outputFile $ \outputFilePtr ->
        c_reussir_bridge_compile_for_native_machine
          mlirPtr
          sourceNamePtr
          outputFilePtr
          (outputTargetToC target)
          (optOptionToC opt)
          (logLevelToC logLevel)

-- | Get the native target triple for the current machine
--
-- This function returns the LLVM target triple string for the native machine.
-- The returned string describes the architecture, vendor, OS, and environment
-- (e.g., "x86_64-unknown-linux-gnu").
--
-- Example:
--
-- @
-- triple <- getNativeTargetTriple
-- putStrLn $ "Native target: " ++ triple
-- @
getNativeTargetTriple :: IO String
getNativeTargetTriple = do
  triplePtr <- c_reussir_bridge_alloc_native_target_triple
  triple <- peekCString triplePtr
  c_reussir_bridge_free_native_target_triple triplePtr
  pure triple
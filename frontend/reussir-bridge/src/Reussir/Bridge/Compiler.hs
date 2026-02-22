{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Reussir.Bridge.Compiler (
    -- * Compilation
    compileForNativeMachine,
    compileForTarget,
    compileProgram,
    hasTPDE,
    getNativeTargetTriple,
    getNativeTargetCPU,
    getNativeTargetFeatures,
)
where

import Data.ByteString (ByteString, empty, useAsCString)
import Foreign.C.String
import Foreign.C.Types

import Data.ByteString.Unsafe qualified as BSU

import Reussir.Bridge.Types

--------------------------------------------------------------------------------
-- C API
--------------------------------------------------------------------------------

foreign import capi "Reussir/Bridge.h reussir_bridge_has_tpde"
    hasTPDE :: IO Bool

-- need to free to free the CString
foreign import capi "Reussir/Bridge.h reussir_bridge_get_default_target_triple"
    c_reussir_bridge_get_default_target_triple :: IO CString

-- need to free to free the CString
foreign import capi "Reussir/Bridge.h reussir_bridge_get_default_target_cpu"
    c_reussir_bridge_get_default_target_cpu :: IO CString

-- need to free to first free the CString, then free the Array
-- terminated by nullPtr
foreign import capi "Reussir/Bridge.h reussir_bridge_get_default_target_features"
    c_reussir_bridge_get_default_target_features :: IO CString

foreign import capi "Reussir/Bridge.h reussir_bridge_compile_for_target"
    c_reussir_bridge_compile_for_target ::
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
        -- | target triple
        CString ->
        -- | target CPU
        CString ->
        -- | target feature string
        CString ->
        -- | target code model
        CInt ->
        -- | target relocation model
        CInt ->
        IO ()

getNativeTargetTriple :: IO ByteString
getNativeTargetTriple = do
    targetTriple <- c_reussir_bridge_get_default_target_triple
    BSU.unsafePackMallocCString targetTriple

getNativeTargetCPU :: IO ByteString
getNativeTargetCPU = do
    targetCPU <- c_reussir_bridge_get_default_target_cpu
    BSU.unsafePackMallocCString targetCPU

getNativeTargetFeatures :: IO ByteString
getNativeTargetFeatures = do
    targetFeatures <- c_reussir_bridge_get_default_target_features
    BSU.unsafePackMallocCString targetFeatures

compileForNativeMachine ::
    -- | MLIR module content (must be null-terminated or will be null-terminated)
    ByteString ->
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
compileForNativeMachine mlirModule sourceName outputFile target opt logLevel = do
    targetTriple <- getNativeTargetTriple
    targetCPU <- getNativeTargetCPU
    targetFeatures <- getNativeTargetFeatures
    compileProgram
        Program
            { mlirModule
            , sourceName
            , outputFile = outputFile
            , outputTarget = target
            , opt = opt
            , logLevel = logLevel
            , targetTriple = targetTriple
            , targetCPU = targetCPU
            , targetFeatures = targetFeatures
            , targetCodeModel = CodeModelDefault
            , targetRelocationModel = RelocationModelPIC
            }

compileForTarget ::
    -- | MLIR module content
    ByteString ->
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
    -- | Optional target triple (Nothing = native)
    Maybe ByteString ->
    -- | Optional target CPU (Nothing = native)
    Maybe ByteString ->
    -- | Optional target features (Nothing = native)
    Maybe ByteString ->
    IO ()
compileForTarget mlirModule sourceName outputFile target opt logLevel mTriple mCPU mFeatures = do
    triple <- maybe getNativeTargetTriple pure mTriple
    -- When a custom triple is specified, default CPU/features to empty strings
    -- so LLVM picks appropriate defaults for the target architecture.
    -- Using native CPU/features with a foreign triple causes crashes.
    let isCustomTriple = case mTriple of Nothing -> False; Just _ -> True
    cpu <- maybe (if isCustomTriple then pure empty else getNativeTargetCPU) pure mCPU
    features <- maybe (if isCustomTriple then pure empty else getNativeTargetFeatures) pure mFeatures
    compileProgram
        Program
            { mlirModule
            , sourceName
            , outputFile = outputFile
            , outputTarget = target
            , opt = opt
            , logLevel = logLevel
            , targetTriple = triple
            , targetCPU = cpu
            , targetFeatures = features
            , targetCodeModel = CodeModelDefault
            , targetRelocationModel = RelocationModelPIC
            }

compileProgram :: Program -> IO ()
compileProgram
    Program
        { mlirModule = mlirModule
        , sourceName = sourceName
        , outputFile = outputFile
        , outputTarget = outputTarget
        , opt = opt
        , logLevel = logLevel
        , targetTriple = targetTriple
        , targetCPU = targetCPU
        , targetFeatures = featuresMap
        , targetCodeModel = targetCodeModel
        , targetRelocationModel = targetRelocationModel
        } =
        useAsCString mlirModule $ \mlirPtr ->
            withCString sourceName $ \sourceNamePtr ->
                withCString outputFile $ \outputFilePtr ->
                    useAsCString targetTriple $ \targetTriplePtr ->
                        useAsCString targetCPU $ \targetCPUPtr ->
                            useAsCString featuresMap $ \featuresPtr ->
                                c_reussir_bridge_compile_for_target
                                    mlirPtr
                                    sourceNamePtr
                                    outputFilePtr
                                    (outputTargetToC outputTarget)
                                    (optOptionToC opt)
                                    (logLevelToC logLevel)
                                    targetTriplePtr
                                    targetCPUPtr
                                    featuresPtr
                                    (codeModelToC targetCodeModel)
                                    (relocationModelToC targetRelocationModel)

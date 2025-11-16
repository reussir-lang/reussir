{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Reussir.Bridge.Compiler (
    -- * Compilation
    compileForNativeMachine,
    compileProgram,
    hasTPDE,
    getNativeTargetTriple,
    getNativeTargetCPU,
)
where

import Data.ByteString (ByteString, useAsCString)
import Data.ByteString.Unsafe qualified as BSU
import Foreign.C.String
import Foreign.C.Types
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
            , targetRelocationModel = RelocationModelDefault
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


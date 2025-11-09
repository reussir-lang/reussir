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

    -- * Compilation
    compileForNativeMachine,
    compileProgram,
    CodeModel (..),
    RelocationModel (..),
    Program (..),
    hasTPDE,
    getNativeTargetTriple,
    getNativeTargetCPU,
)
where

import Data.ByteString (ByteString, useAsCString)
import Data.ByteString.Unsafe qualified as BSU
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

--------------------------------------------------------------------------------
-- Code Model
--------------------------------------------------------------------------------
data CodeModel
    = -- | Tiny code model
      CodeModelTiny
    | -- | Small code model
      CodeModelSmall
    | -- | Kernel code model
      CodeModelKernel
    | -- | Medium code model
      CodeModelMedium
    | -- | Large code model
      CodeModelLarge
    | -- | Default code model
      CodeModelDefault
    deriving (Eq, Show, Enum)

codeModelToC :: CodeModel -> CInt
codeModelToC CodeModelTiny = 0
codeModelToC CodeModelSmall = 1
codeModelToC CodeModelKernel = 2
codeModelToC CodeModelMedium = 3
codeModelToC CodeModelLarge = 4
codeModelToC CodeModelDefault = 5

--------------------------------------------------------------------------------
-- Relocation Model
--------------------------------------------------------------------------------
-- Static, PIC_, DynamicNoPIC, ROPI, RWPI, ROPI_RWPI
data RelocationModel
    = -- | Static relocation model
      RelocationModelStatic
    | -- | PIC relocation model
      RelocationModelPIC
    | -- | Dynamic relocation model
      RelocationModelDynamic
    | -- | ROPI relocation model
      RelocationModelROPI
    | -- | RWPI relocation model
      RelocationModelRWPI
    | -- | ROPI_RWPI relocation model
      RelocationModelROPI_RWPI
    | -- | Default relocation model
      RelocationModelDefault
    deriving (Eq, Show, Enum)

relocationModelToC :: RelocationModel -> CInt
relocationModelToC RelocationModelStatic = 0
relocationModelToC RelocationModelPIC = 1
relocationModelToC RelocationModelDynamic = 2
relocationModelToC RelocationModelROPI = 3
relocationModelToC RelocationModelRWPI = 4
relocationModelToC RelocationModelROPI_RWPI = 5
relocationModelToC RelocationModelDefault = 6

data Program = Program
    { mlirModule :: ByteString
    , sourceName :: String
    , outputFile :: FilePath
    , outputTarget :: OutputTarget
    , opt :: OptOption
    , logLevel :: LogLevel
    , targetTriple :: ByteString
    , targetCPU :: ByteString
    , targetFeatures :: ByteString
    , targetCodeModel :: CodeModel
    , targetRelocationModel :: RelocationModel
    }
    deriving (Eq, Show)

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

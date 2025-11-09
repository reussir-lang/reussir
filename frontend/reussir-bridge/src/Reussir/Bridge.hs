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

    -- * Target Information
    FeaturesMap,
    enableFeature,
    disableFeature,
    removeFeature,
    emptyFeaturesMap,
    isFeatureEnabled,
    isFeatureDisabled,
    containsFeature,
    CodeModel (..),
    RelocationModel (..),
    Target (..),
)
where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.ByteString.Unsafe qualified as BSU
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as H
import Foreign (withArray0)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr)

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

-------------------------------------------------------------------------------
-- Haskell API
-------------------------------------------------------------------------------

{- | Compile MLIR module for the native machine target

This function takes an MLIR module as a string, compiles it to the specified
output format with the given optimization level, and writes the result to
the output file.

Example:

@
compileForNativeMachine
  "module {}"
  "example.mlir"
  "output.o"
  OutputObject
  OptDefault
  LogInfo
@
-}
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
compileForNativeMachine mlirModule sourceName outputFile target opt logLevel =
    unsafeUseAsCString mlirModule $ \mlirPtr ->
        withCString sourceName $ \sourceNamePtr ->
            withCString outputFile $ \outputFilePtr ->
                c_reussir_bridge_compile_for_native_machine
                    mlirPtr
                    sourceNamePtr
                    outputFilePtr
                    (outputTargetToC target)
                    (optOptionToC opt)
                    (logLevelToC logLevel)

--------------------------------------------------------------------------------
-- Feature Flags
--------------------------------------------------------------------------------

newtype FeaturesMap = FeaturesMap (HashMap ByteString Bool)
    deriving (Eq, Show)

enableFeature :: FeaturesMap -> ByteString -> FeaturesMap
enableFeature (FeaturesMap features) feature = FeaturesMap (H.insert feature True features)

disableFeature :: FeaturesMap -> ByteString -> FeaturesMap
disableFeature (FeaturesMap features) feature = FeaturesMap (H.insert feature False features)

removeFeature :: FeaturesMap -> ByteString -> FeaturesMap
removeFeature (FeaturesMap features) feature = FeaturesMap (H.delete feature features)

emptyFeaturesMap :: FeaturesMap
emptyFeaturesMap = FeaturesMap H.empty

isFeatureEnabled :: FeaturesMap -> ByteString -> Bool
isFeatureEnabled (FeaturesMap features) feature = H.lookupDefault False feature features

isFeatureDisabled :: FeaturesMap -> ByteString -> Bool
isFeatureDisabled (FeaturesMap features) feature = H.lookupDefault True feature features

containsFeature :: FeaturesMap -> ByteString -> Bool
containsFeature (FeaturesMap features) feature = H.member feature features

featureNamesToC :: [ByteString] -> (Ptr CString -> IO a) -> IO a
featureNamesToC bsList action =
    go bsList [] $ \cstrs ->
        withArray0 nullPtr cstrs action
  where
    go [] acc cont = cont (reverse acc)
    go (b : bs) acc cont =
        BSU.unsafeUseAsCString b $ \cstr ->
            go bs (cstr : acc) cont

featureFlagsToC :: [Bool] -> (Ptr CChar -> IO a) -> IO a
featureFlagsToC = withArray0 (-1) . map (\b -> if b then 1 else 0)

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
    deriving (Eq, Show, Enum)

codeModelToC :: CodeModel -> CInt
codeModelToC CodeModelTiny = 0
codeModelToC CodeModelSmall = 1
codeModelToC CodeModelKernel = 2
codeModelToC CodeModelMedium = 3
codeModelToC CodeModelLarge = 4

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
    deriving (Eq, Show, Enum)

relocationModelToC :: RelocationModel -> CInt
relocationModelToC RelocationModelStatic = 0
relocationModelToC RelocationModelPIC = 1
relocationModelToC RelocationModelDynamic = 2
relocationModelToC RelocationModelROPI = 3
relocationModelToC RelocationModelRWPI = 4
relocationModelToC RelocationModelROPI_RWPI = 5

data Target = Target
    { targetTriple :: ByteString
    , targetCPU :: ByteString
    , targetFeatures :: FeaturesMap
    , targetCodeModel :: Maybe CodeModel
    , targetRelocationModel :: Maybe RelocationModel
    }
    deriving (Eq, Show)

foreign import capi "Reussir/Bridge.h reussir_bridge_compile_for_target"
    c_reussir_bridge_compile_for_target ::
        -- | mlir_module
        CString ->
        -- | source_name
        CString ->
        -- | output_file
        CString ->
        -- | opt (ReussirOptOption)
        CInt ->
        -- | log_level (ReussirLogLevel)
        CInt ->
        -- | target triple
        CString ->
        -- | target CPU
        CString ->
        -- | target feature names
        Ptr CString ->
        -- | target feature flags
        Ptr Bool ->
        -- | target code model
        CInt ->
        -- | target relocation model
        CInt ->
        IO ()

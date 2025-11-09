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
    Program (..),
    hasTPDE,
    getNativeTargetTriple,
    getNativeTargetCPU,
    getNativeTargetFeatureMap,
)
where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.ByteString.Unsafe qualified as BSU
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as H
import Data.Int (Int8)
import Foreign (withArray0)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal (peekArray0)
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
    targetFeatures <- getNativeTargetFeatureMap
    compileProgram
        Program
            { mlirModule = mlirModule
            , sourceName = sourceName
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

featureFlagsToC :: [Bool] -> (Ptr Int8 -> IO a) -> IO a
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
    , targetFeatures :: FeaturesMap
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

foreign import ccall "free"
    c_free :: Ptr a -> IO ()

-- need to free to free the CString
foreign import capi "Reussir/Bridge.h reussir_bridge_get_default_target_cpu"
    c_reussir_bridge_get_default_target_cpu :: IO CString

-- need to free to first free the CString, then free the Array
-- terminated by nullPtr
foreign import capi "Reussir/Bridge.h reussir_bridge_get_default_target_features"
    c_reussir_bridge_get_default_target_features :: IO (Ptr CString)

-- terminated by -1
foreign import capi "Reussir/Bridge.h reussir_bridge_get_default_target_feature_flags"
    c_reussir_bridge_get_default_target_feature_flags :: IO (Ptr Int8)

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
        -- | target feature names (null-terminated array)
        Ptr CString ->
        -- | target feature flags (0/1, terminated by -1)
        Ptr Int8 ->
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

getNativeTargetFeatureMap :: IO FeaturesMap
getNativeTargetFeatureMap = do
    featureNames <- c_reussir_bridge_get_default_target_features
    featureFlags <- c_reussir_bridge_get_default_target_feature_flags
    unmarshalled <- peekArray0 nullPtr featureNames
    names <- mapM BSU.unsafePackMallocCString unmarshalled
    c_free featureNames -- free array wrapper
    unmarshalledFeatureFlags <- peekArray0 (-1) featureFlags
    let flags = map (== 1) unmarshalledFeatureFlags
    c_free featureFlags -- free array wrapper
    return (FeaturesMap (H.fromList $ zip names flags))

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
        , targetFeatures = FeaturesMap featuresMap
        , targetCodeModel = targetCodeModel
        , targetRelocationModel = targetRelocationModel
        } = do
        unsafeUseAsCString mlirModule $ \mlirPtr ->
            withCString sourceName $ \sourceNamePtr ->
                withCString outputFile $ \outputFilePtr ->
                    unsafeUseAsCString targetTriple $ \targetTriplePtr ->
                        unsafeUseAsCString targetCPU $ \targetCPUPtr -> do
                            let keyVals = H.toList featuresMap
                            let keys = map fst keyVals
                            let vals = map snd keyVals
                            featureNamesToC keys $ \featureNamesPtr ->
                                featureFlagsToC vals $ \featureFlagsPtr ->
                                    c_reussir_bridge_compile_for_target
                                        mlirPtr
                                        sourceNamePtr
                                        outputFilePtr
                                        (outputTargetToC outputTarget)
                                        (optOptionToC opt)
                                        (logLevelToC logLevel)
                                        targetTriplePtr
                                        targetCPUPtr
                                        featureNamesPtr
                                        featureFlagsPtr
                                        (codeModelToC targetCodeModel)
                                        (relocationModelToC targetRelocationModel)

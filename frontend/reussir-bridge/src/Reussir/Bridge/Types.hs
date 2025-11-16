{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Reussir.Bridge.Types (
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
)
where

import Data.ByteString (ByteString)
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

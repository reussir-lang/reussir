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
    compileProgram,
    hasTPDE,
    getNativeTargetTriple,
    getNativeTargetCPU,
)
where

import Reussir.Bridge.Compiler
import Reussir.Bridge.Types

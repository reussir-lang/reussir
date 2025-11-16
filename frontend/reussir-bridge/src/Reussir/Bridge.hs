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

    -- * JIT Engine
    module Reussir.Bridge.JITEngine,
)
where

import Reussir.Bridge.Compiler
import Reussir.Bridge.JITEngine
import Reussir.Bridge.Types

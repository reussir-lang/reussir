{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
   REPL Support for Reussir
   ========================

   This module provides REPL (Read-Eval-Print Loop) support for the Reussir language.
   It maintains compilation state across REPL interactions and supports:

   1. Statement mode (:stmt): Add new definitions (functions, records) to the context
   2. Eval mode (:eval): Evaluate expressions and print their results

   The REPL maintains the SemiContext and incrementally compiles new definitions
   using the JIT engine. Strict evaluation is used.
-}
module Reussir.Core.REPL (
    -- * REPL State
    ReplState (..),
    ReplError (..),

    -- * Initialization
    initReplState,

    -- * REPL Operations
    addDefinition,
    compileExpression,

    -- * Type Utilities
    isPrimitiveType,
    typeToResultKind,
    ResultKind (..),
) where

import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Effectful (Eff, IOE, inject, liftIO, runEff, (:>))
import Effectful.Prim (runPrim)
import Effectful.Prim.IORef.Strict (Prim)
import Effectful.State.Static.Local (runState)
import Log (LogLevel (..))
import Reussir.Codegen.Context.Symbol (Symbol, verifiedSymbol)
import Reussir.Codegen.Global (Global (..))
import Reussir.Diagnostic.Display (displayReport)
import Reussir.Diagnostic.Repository (Repository, createRepository)
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (..))
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)

import Data.HashTable.IO qualified as H
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful.Log qualified as L
import Effectful.State.Static.Local qualified as State
import Reussir.Bridge qualified as B
import Reussir.Codegen qualified as IR
import Reussir.Codegen.Context qualified as IR
import Reussir.Parser.Types.Expr qualified as SynExpr
import Reussir.Parser.Types.Stmt qualified as Syn

import Reussir.Core.Data.Class (Class (..))
import Reussir.Core.Data.FP (FloatingPointType (..))
import Reussir.Core.Data.Full.Context (FullContext (..))
import Reussir.Core.Data.Generic (GenericSolution)
import Reussir.Core.Data.Integral (IntegralType (..))
import Reussir.Core.Data.Semi.Context (SemiContext (..), SemiEff)
import Reussir.Core.Full.Expr (convertSemiExpr)
import Reussir.Core.Full.Function (convertAllSemiFunctions)
import Reussir.Core.Full.Record (convertSemiRecordTable)
import Reussir.Core.Full.Type (convertSemiType)
import Reussir.Core.Lowering.Context (
    createLoweringContext,
    runLoweringToModule,
 )
import Reussir.Core.Lowering.Function (lowerFunction)
import Reussir.Core.Lowering.Record (lowerRecord)
import Reussir.Core.Semi.Context (
    emptyLocalSemiContext,
    emptySemiContext,
    populateRecordFields,
    runUnification,
    scanStmt,
 )
import Reussir.Core.Semi.FlowAnalysis (
    analyzeGenericFlowInExpr,
    solveAllGenerics,
 )
import Reussir.Core.Semi.Tyck (checkFuncType, elimTypeHoles, inferType)
import Reussir.Core.Semi.Unification (force, satisfyBounds, unify)
import Reussir.Core.String (getAllStrings, mangleStringToken)

import Reussir.Core.Data.Full.Function qualified as Full
import Reussir.Core.Data.Semi.Expr qualified as Semi
import Reussir.Core.Data.Semi.Function qualified as SemiFunc
import Reussir.Core.Data.Semi.Type qualified as Semi
import Reussir.Core.Full.Context qualified as Full
import qualified Reussir.Codegen.Trampoline as IR

--------------------------------------------------------------------------------
-- REPL Types
--------------------------------------------------------------------------------

-- | Errors that can occur during REPL operations
data ReplError
    = TypeCheckError String
    | ParseError String
    | CompilationError String
    | ElaborationError String
    deriving (Show, Eq)

-- | Result kind for expressions - determines how to print the result
data ResultKind
    = ResultI8
    | ResultI16
    | ResultI32
    | ResultI64
    | ResultU8
    | ResultU16
    | ResultU32
    | ResultU64
    | ResultF16
    | ResultF32
    | ResultF64
    | ResultBool
    | ResultUnit
    | ResultStr
    | -- | For non-primitive types, store type description
      ResultOther T.Text
    deriving (Show, Eq)

-- | The REPL state maintains compilation context across interactions
data ReplState = ReplState
    { replSemiContext :: SemiContext
    -- ^ Semi-elaboration context with scanned definitions
    , replCompiledFunctions :: H.CuckooHashTable Symbol ()
    -- ^ Track which functions have been compiled
    , replCounter :: Int
    -- ^ Counter for generating unique REPL expression names
    , replLogLevel :: B.LogLevel
    -- ^ Log level for diagnostics
    , replFilePath :: FilePath
    -- ^ Virtual file path for REPL input
    , replGenericSolution :: GenericSolution
    -- ^ Solution for generics from flow analysis
    }

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Global counter for unique logger names
{-# NOINLINE loggerCounter #-}
loggerCounter :: IORef Int
loggerCounter = unsafePerformIO $ newIORef 0

-- | Generate a unique logger name to avoid spdlog conflicts
uniqueLoggerName :: IO String
uniqueLoggerName = do
    n <- atomicModifyIORef' loggerCounter $ \x -> (x + 1, x)
    return $ "REPL-" ++ show n

-- | Create an empty repository for REPL (since there's no real file)
emptyReplRepository :: (IOE :> es) => Eff es Repository
emptyReplRepository = createRepository []

-- | Initialize a fresh REPL state
initReplState :: B.LogLevel -> FilePath -> IO ReplState
initReplState logLevel filePath = do
    loggerName <- uniqueLoggerName
    B.withReussirLogger logLevel loggerName $ \logger -> do
        runEff $ L.runLog "REPL" logger (toEffLogLevel logLevel) $ runPrim $ do
            semiCtx <- emptySemiContext logLevel filePath
            compiledFuncs <- liftIO H.new
            genericSol <- liftIO H.new
            return
                ReplState
                    { replSemiContext = semiCtx
                    , replCompiledFunctions = compiledFuncs
                    , replCounter = 0
                    , replLogLevel = logLevel
                    , replFilePath = filePath
                    , replGenericSolution = genericSol
                    }

-- | Convert Bridge LogLevel to effectful LogLevel
toEffLogLevel :: B.LogLevel -> LogLevel
toEffLogLevel B.LogError = LogAttention
toEffLogLevel B.LogWarning = LogAttention
toEffLogLevel B.LogInfo = LogInfo
toEffLogLevel B.LogDebug = LogTrace
toEffLogLevel B.LogTrace = LogTrace

--------------------------------------------------------------------------------
-- Statement Processing
--------------------------------------------------------------------------------

-- | Add a new definition (function, record, etc.) to the REPL context
addDefinition ::
    ReplState ->
    Syn.Stmt ->
    IO (Either ReplError ReplState)
addDefinition state stmt = do
    let logLevel = replLogLevel state
    loggerName <- uniqueLoggerName

    B.withReussirLogger logLevel loggerName $ \logger -> do
        runEff $ L.runLog "REPL" logger (toEffLogLevel logLevel) $ runPrim $ do
            -- Run semi-elaboration in the existing context
            (mGenericSol, newSemiCtx) <- runState (replSemiContext state) $ do
                -- Scan the statement to add to context
                inject $ scanStmt stmt

                -- Populate record fields if applicable
                inject $ populateRecordFields stmt

                -- If it's a function, type check it
                case unspanStmt stmt of
                    Syn.FunctionStmt f -> do
                        _ <- inject $ checkFuncType f
                        return ()
                    _ -> return ()

                -- Solve generics after each addition
                inject solveAllGenerics

            -- Check for elaboration errors
            if translationHasFailed newSemiCtx
                then do
                    -- Display errors to stderr
                    repository <- emptyReplRepository
                    forM_ (translationReports newSemiCtx) $ \report -> do
                        displayReport report repository 0 stderr
                    -- Clear the error state for next interaction but keep the context
                    return $ Left $ ElaborationError "Elaboration failed"
                else do
                    -- Update generic solution if we got a new one
                    case mGenericSol of
                        Just newSol -> do
                            -- Merge new solutions into existing
                            newEntries <- liftIO $ H.toList newSol
                            forM_ newEntries $ \(k, v) -> do
                                liftIO $ H.insert (replGenericSolution state) k v
                        Nothing -> return ()
                    return $
                        Right
                            state
                                { replSemiContext = newSemiCtx
                                }
  where
    unspanStmt (Syn.SpannedStmt (WithSpan s _ _)) = unspanStmt s
    unspanStmt s = s

--------------------------------------------------------------------------------
-- Expression Evaluation
--------------------------------------------------------------------------------

{- | Compile an expression for JIT execution
Returns the MLIR module text (as ByteString), updated state, and result kind
-}
compileExpression ::
    ReplState ->
    -- | Original source text (for error reporting)
    T.Text ->
    -- | Parsed expression
    SynExpr.Expr ->
    IO (Either ReplError (ByteString, ReplState, ResultKind))
compileExpression state _sourceText expr = do
    let logLevel = replLogLevel state
    let counter = replCounter state
    let exprFuncName = "__repl_expr_" <> T.pack (show counter)
    loggerName <- uniqueLoggerName

    B.withReussirLogger logLevel loggerName $ \logger -> do
        runEff $ L.runLog "REPL" logger (toEffLogLevel logLevel) $ runPrim $ do
            -- Run type inference in the existing semi context
            -- We need to run SemiEff actions, so we use withFreshLocalContextAndResult
            ((elaboratedExpr, exprType, mGenericSol), finalSemiCtx) <- runState (replSemiContext state) $ do
                -- Create a fresh local context and run SemiEff actions
                localCtx <- emptyLocalSemiContext
                ((elab, ty), _finalLocalCtx) <- runState localCtx $ do
                    -- Infer the type of the expression
                    inferredExpr <- inject $ inferType expr

                    -- Force and resolve type holes
                    let exprTy = Semi.exprType inferredExpr
                    resolvedTy <- inject $ runUnification $ force exprTy

                    -- Try to resolve numeric type holes to concrete types
                    finalTy <- inject $ resolveTypeHolesForRepl resolvedTy

                    -- Update expression with resolved type and eliminate holes
                    let updatedExpr = inferredExpr{Semi.exprType = finalTy}
                    finalExpr <- inject $ elimTypeHoles updatedExpr

                    return (finalExpr, finalTy)

                -- Check for new generic instantiations required by the expression
                inject $ analyzeGenericFlowInExpr elab
                -- Solve generics to update the solution
                mGenericSol <- inject solveAllGenerics

                return (elab, ty, mGenericSol)

            -- Check for type errors
            if translationHasFailed finalSemiCtx
                then do
                    repository <- emptyReplRepository
                    forM_ (translationReports finalSemiCtx) $ \report -> do
                        displayReport report repository 0 stderr
                    return $ Left $ TypeCheckError "Type checking failed"
                else do
                    -- Update generic solution if we got a new one
                    case mGenericSol of
                        Just newSol -> do
                            -- Merge new solutions into existing (replGenericSolution)
                            newEntries <- liftIO $ H.toList newSol
                            forM_ newEntries $ \(k, v) -> do
                                liftIO $ H.insert (replGenericSolution state) k v
                        Nothing -> return ()

                    let resultKind = typeToResultKind exprType

                    -- Generate MLIR wrapper for the expression
                    mlirResult <-
                        generateExpressionModule
                            exprFuncName
                            elaboratedExpr
                            exprType
                            logLevel
                            state

                    case mlirResult of
                        Left err -> return $ Left err
                        Right mlirBytes -> do
                            let newState =
                                    state
                                        { replCounter = counter + 1
                                        , replSemiContext =
                                            finalSemiCtx
                                                { translationHasFailed = False
                                                , translationReports = []
                                                }
                                        }
                            return $ Right (mlirBytes, newState, resultKind)

{- | Resolve type holes with Integral/FloatingPoint bounds to concrete types
This is used in SemiEff context where we have access to LocalSemiContext
-}
resolveTypeHolesForRepl :: Semi.Type -> SemiEff Semi.Type
resolveTypeHolesForRepl ty = do
    forcedTy <- runUnification $ force ty
    case forcedTy of
        Semi.TypeHole _ -> do
            -- Check if the hole satisfies Integral bound
            isIntegral <-
                runUnification $
                    satisfyBounds forcedTy [Class $ Path "Integral" []]
            if isIntegral
                then do
                    -- Unify with i64
                    _ <- runUnification $ unify forcedTy (Semi.TypeIntegral (Signed 64))
                    return $ Semi.TypeIntegral (Signed 64)
                else do
                    -- Check for FloatingPoint bound
                    isFloat <-
                        runUnification $
                            satisfyBounds forcedTy [Class $ Path "FloatingPoint" []]
                    if isFloat
                        then do
                            -- Unify with f64
                            _ <- runUnification $ unify forcedTy (Semi.TypeFP (IEEEFloat 64))
                            return $ Semi.TypeFP (IEEEFloat 64)
                        else return forcedTy
        _ -> return forcedTy

{- | Generate an MLIR module containing a wrapper function for the expression
Also includes all previously defined functions so they can be called
-}
generateExpressionModule ::
    (IOE :> es, Prim :> es, L.Log :> es) =>
    -- | Function name for the wrapper
    T.Text ->
    -- | Elaborated expression
    Semi.Expr ->
    -- | Expression type
    Semi.Type ->
    -- | Log level
    B.LogLevel ->
    -- | REPL state
    ReplState ->
    Eff es (Either ReplError ByteString)
generateExpressionModule funcName semiExpr exprType logLevel state = do
    let semiCtx = replSemiContext state
    let semiRecords = knownRecords semiCtx
    let filePath = replFilePath state
    let genericSol = replGenericSolution state

    -- Create empty generic map (REPL expressions shouldn't have unbound generics)
    let genericMap = IntMap.empty

    -- Convert Semi type to Full type
    tyResult <- convertSemiType (0, 0) genericMap semiRecords exprType

    case tyResult of
        Left _errs -> return $ Left $ CompilationError "Failed to convert expression type"
        Right fullType -> do
            -- Create a Full context and convert all Semi functions to Full
            emptyCtx <- Full.emptyFullContext filePath

            -- Set up the semiRecords in the full context for function conversion
            let fullCtxWithRecords = emptyCtx{ctxSemiRecords = semiRecords}

            -- Convert all functions from SemiContext to FullContext
            finalFullCtx <- State.execState fullCtxWithRecords $ do
                -- Convert records first (if any)
                errs <- inject $ convertSemiRecordTable semiRecords genericSol
                forM_ errs (inject . Full.addError)

                -- Convert all semi functions
                when (null errs) $ do
                    funcProtos <- liftIO $ H.toList (SemiFunc.functionProtos $ functions semiCtx)
                    inject $ convertAllSemiFunctions (map snd funcProtos) genericSol

            -- Convert the expression in the context with all functions available
            (fullExpr, _) <- State.runState finalFullCtx $
                inject $
                    Full.withFreshLocalContext $
                        Full.withGenericMap genericMap $ do
                            convertSemiExpr semiExpr

            -- Create a wrapper function that returns the expression value
            let wrapperFunc =
                    Full.Function
                        { Full.funcVisibility = Syn.Private
                        , Full.funcName = verifiedSymbol funcName
                        , Full.funcRawPath = Path (Identifier funcName) []
                        , Full.funcInstantiatedTyArgs = []
                        , Full.funcParams = []
                        , Full.funcReturnType = fullType
                        , Full.funcIsRegional = False
                        , Full.funcBody = Just fullExpr
                        , Full.funcSpan = Nothing
                        }

            -- Add wrapper to the function table
            liftIO $
                H.insert (ctxFunctions finalFullCtx) (Full.funcName wrapperFunc) wrapperFunc

            -- Create target spec for codegen
            let targetSpec =
                    IR.TargetSpec
                        { IR.programName = "repl"
                        , IR.outputPath = ""
                        , IR.optimization = B.OptTPDE
                        , IR.outputTarget = B.OutputObject
                        , IR.logLevel = logLevel
                        , IR.moduleFilePath = filePath
                        , IR.targetTriple = Nothing
                        , IR.targetCPU = Nothing
                        , IR.targetFeatures = Nothing
                        }

            -- Create lowering context with all functions
            repository <- emptyReplRepository
            let stringUniq = stringUniqifier semiCtx

            loweringCtx <-
                createLoweringContext
                    repository
                    (ctxFunctions finalFullCtx)
                    (ctxRecords finalFullCtx)
                    stringUniq
                    (ctxTrampolines finalFullCtx)
                    targetSpec

            -- Lower functions
            -- We need to lower:
            -- 1. The wrapper function (always new)
            -- 2. New functions that haven't been compiled yet
            -- 3. Declarations for already compiled functions (so the linker is happy)

            allFuncs <- liftIO $ H.toList (ctxFunctions finalFullCtx)
            compiledFuncs <- map fst <$> liftIO (H.toList (replCompiledFunctions state))

            irModule <- runLoweringToModule loweringCtx $ do
                -- Lower all records first
                allRecords <- liftIO $ H.toList (ctxRecords finalFullCtx)
                forM_ allRecords $ \(_, record) -> lowerRecord record

                -- Lower all strings from the string table
                strList <- getAllStrings stringUniq
                forM_ strList $ \(strVal, token) -> do
                    let symbol = verifiedSymbol $ mangleStringToken token
                    State.modify $ \mod' ->
                        let global = GlobalString symbol strVal
                         in mod'{IR.globals = global : IR.globals mod'}

                -- Lower all functions
                forM_ allFuncs $ \(_, func) -> do
                    if Full.funcName func `elem` compiledFuncs
                        then do
                            -- Already compiled, just emit declaration (remove body)
                            -- This ensures we can call it but don't redefine it
                            let declFunc = func{Full.funcBody = Nothing}
                            lowerFunction declFunc
                        else do
                            -- New function, lower fully
                            lowerFunction func
                let funcSymbol = verifiedSymbol funcName
                let funcTrampoline = funcName <> "_trampoline"
                let funcTrampolineSymbol = verifiedSymbol funcTrampoline
                let trampoline' = IR.Trampoline funcTrampolineSymbol funcSymbol "C"
                mod' <- State.get
                let updatedMod = mod'{IR.trampolines = trampoline' : IR.trampolines mod'}
                State.put updatedMod

            -- Update compiled functions set
            forM_ allFuncs $ \(_, func) -> do
                liftIO $ H.insert (replCompiledFunctions state) (Full.funcName func) ()

            -- Emit to MLIR text
            mlirText <- IR.emitModuleToText irModule
            return $ Right $ TE.encodeUtf8 mlirText

--------------------------------------------------------------------------------
-- Type Utilities
--------------------------------------------------------------------------------

-- | Check if a type is a primitive type that can be printed
isPrimitiveType :: Semi.Type -> Bool
isPrimitiveType (Semi.TypeIntegral _) = True
isPrimitiveType (Semi.TypeFP _) = True
isPrimitiveType Semi.TypeBool = True
isPrimitiveType Semi.TypeUnit = True
isPrimitiveType Semi.TypeStr = True
isPrimitiveType _ = False

-- | Convert a Semi type to a ResultKind for printing
typeToResultKind :: Semi.Type -> ResultKind
typeToResultKind (Semi.TypeIntegral (Signed 8)) = ResultI8
typeToResultKind (Semi.TypeIntegral (Signed 16)) = ResultI16
typeToResultKind (Semi.TypeIntegral (Signed 32)) = ResultI32
typeToResultKind (Semi.TypeIntegral (Signed 64)) = ResultI64
typeToResultKind (Semi.TypeIntegral (Unsigned 8)) = ResultU8
typeToResultKind (Semi.TypeIntegral (Unsigned 16)) = ResultU16
typeToResultKind (Semi.TypeIntegral (Unsigned 32)) = ResultU32
typeToResultKind (Semi.TypeIntegral (Unsigned 64)) = ResultU64
typeToResultKind (Semi.TypeFP (IEEEFloat 16)) = ResultOther "f16" -- Avoid unsafe read for now
typeToResultKind (Semi.TypeFP (IEEEFloat 32)) = ResultF32
typeToResultKind (Semi.TypeFP (IEEEFloat 64)) = ResultF64
typeToResultKind Semi.TypeBool = ResultBool
typeToResultKind Semi.TypeUnit = ResultUnit
typeToResultKind Semi.TypeStr = ResultStr
typeToResultKind ty = ResultOther (T.pack $ show ty)

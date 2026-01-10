{-# LANGUAGE OverloadedStrings #-}

-- | REPL-specific state management for incremental definitions
module Reussir.Core.REPL (
    ReplState (..),
    ReplError (..),
    initReplState,
    addDefinition,
    compileExpression,
) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful (Eff, IOE, inject, runEff, (:>))
import Effectful.Log qualified as L
import Effectful.Prim (Prim, runPrim)
import Effectful.State.Static.Local (execState, runState)
import Effectful.State.Static.Local qualified as State
import GHC.IO.Handle.FD (stderr)
import Log (LogLevel (..))
import Log.Backend.StandardOutput qualified as LogStd
import Reussir.Bridge qualified as B
import Reussir.Codegen qualified as IR
import Reussir.Codegen.Context (TargetSpec (..))
import Reussir.Codegen.Context.Symbol qualified as IR
import Reussir.Codegen.IR qualified as IR
import Reussir.Core.Lowering (addIRInstr, convertType, createLoweringState, lowerExpr, materializeCurrentBlock)
import Reussir.Core.Translation (
    emptyTranslationState,
    scanStmt,
    solveAllGenerics,
    wellTypedExpr,
 )
import Reussir.Core.Tyck (checkFuncType, inferType)
import Reussir.Core.Types.Expr qualified as Sem
import Reussir.Core.Types.Lowering (LoweringSpan (..), LoweringState (..))
import Reussir.Core.Types.Translation (GenericSolution, TranslationState (..))
import Reussir.Diagnostic.Display (displayReport)
import Reussir.Diagnostic.Repository (Repository, addDummyFile, createRepository)
import Reussir.Parser.Types.Expr qualified as Syn
import Reussir.Parser.Types.Lexer (WithSpan (spanValue))
import Reussir.Parser.Types.Stmt qualified as Syn

-- | REPL state containing the accumulated translation state
data ReplState = ReplState
    { replTranslationState :: TranslationState
    , replRepository :: Repository
    , replLogLevel :: B.LogLevel
    , replCounter :: Int -- Counter for unique names
    }

-- | Errors that can occur during REPL operations
data ReplError
    = ParseError String
    | TypeCheckError String
    | CompilationError String
    deriving (Show)

-- | Initialize a fresh REPL state
initReplState :: B.LogLevel -> FilePath -> IO ReplState
initReplState logLevel filePath = do
    runEff $ runPrim $ do
        translationState <- emptyTranslationState logLevel filePath
        -- Use empty repository - we don't need source files for REPL mode
        repository <- createRepository []
        return
            ReplState
                { replTranslationState = translationState
                , replRepository = repository
                , replLogLevel = logLevel
                , replCounter = 0
                }

-- | Add a statement (struct or function definition) to the REPL state
addDefinition :: ReplState -> Syn.Stmt -> IO (Either ReplError ReplState)
addDefinition state stmt = do
    LogStd.withStdOutLogger $ \logger -> do
        runEff $ L.runLog "Reussir.REPL" logger logLevel $ runPrim $ do
            -- Scan the statement to add to index
            (_, state') <- runState (replTranslationState state) $ inject $ do
                scanStmt stmt
                -- Type check if it's a function
                case stripSpan stmt of
                    Syn.FunctionStmt f -> do
                        _ <- inject $ checkFuncType f
                        return ()
                    _ -> return ()

            -- Check for errors
            if null (translationReports state')
                then return $ Right state{replTranslationState = state'}
                else do
                    -- Display errors
                    forM_ (translationReports state') $ \report ->
                        displayReport report (replRepository state) 0 stderr
                    return $ Left $ TypeCheckError "Definition failed type checking"
  where
    logLevel = toEffLogLevel (replLogLevel state)

    stripSpan :: Syn.Stmt -> Syn.Stmt
    stripSpan (Syn.SpannedStmt s) = stripSpan (spanValue s)
    stripSpan s = s

{- | Compile an expression and return the MLIR text
This creates a wrapper function around the expression and compiles it
-}
compileExpression :: ReplState -> T.Text -> Syn.Expr -> IO (Either ReplError (ByteString, ReplState))
compileExpression state sourceText expr = do
    -- Add the source text to the repository so location lookups work
    let updatedRepo = addDummyFile (replRepository state) "<repl>" sourceText
    LogStd.withStdOutLogger $ \logger -> do
        runEff $ L.runLog "Reussir.REPL" logger logLevel $ runPrim $ do
            -- Type check the expression
            (result, state') <- runState (replTranslationState state) $ inject $ do
                typedExpr <- inferType expr
                wellTyped <- wellTypedExpr typedExpr
                return wellTyped

            -- Check for type errors
            if not (null (translationReports state'))
                then do
                    forM_ (translationReports state') $ \report ->
                        displayReport report (replRepository state) 0 stderr
                    return $ Left $ TypeCheckError "Expression failed type checking"
                else do
                    -- Solve generics
                    (mSolution, state'') <- runState state' $ inject solveAllGenerics

                    case mSolution of
                        Nothing -> return $ Left $ TypeCheckError "Failed to solve generics"
                        Just solution -> do
                            -- Create wrapper function and compile
                            let counter = replCounter state
                            let wrapperName = "__repl_expr_" <> T.pack (show counter)
                            let spec =
                                    TargetSpec
                                        { programName = "repl_module"
                                        , outputPath = "<repl>"
                                        , optimization = B.OptTPDE
                                        , outputTarget = B.OutputObject
                                        , logLevel = replLogLevel state
                                        , moduleFilePath = "<repl>"
                                        }
                            let emptyMod = IR.emptyModule spec

                            -- Create lowering state with updated repository
                            loweringState <- createLoweringState "<repl>" updatedRepo emptyMod state''

                            -- Lower to module (placeholder for now)
                            mlirText <- lowerExpressionToModule result wrapperName solution loweringState

                            let newState =
                                    state
                                        { replTranslationState = state''
                                        , replCounter = counter + 1
                                        }
                            return $ Right (TE.encodeUtf8 mlirText, newState)
  where
    logLevel = toEffLogLevel (replLogLevel state)

{- | Helper to lower an expression to a module with a wrapper function
Takes the type-checked semantic expression, not syntactic expression
-}
lowerExpressionToModule ::
    (IOE :> es, Prim :> es, L.Log :> es) =>
    Sem.Expr ->
    T.Text ->
    GenericSolution ->
    LoweringState ->
    Eff es T.Text
lowerExpressionToModule semExpr wrapperName _solution loweringState = do
    -- Run the lowering to generate the IR instructions
    loweringState' <- execState loweringState $ inject $ do
        -- Lower the expression to get the result value
        resultVal <- lowerExpr semExpr

        -- Convert the expression type to IR type
        resultType <- convertType (Sem.exprType semExpr)

        -- Add return instruction
        addIRInstr (IR.Return (Just (resultVal, resultType))) NoSpan

        -- Materialize the block (no block arguments for the wrapper function)
        block <- materializeCurrentBlock []

        -- Create the wrapper function symbol
        let symbol = IR.verifiedSymbol wrapperName

        -- Create the function
        let wrapperFunc =
                IR.Function
                    { IR.funcLinkage = IR.LnkExternal
                    , IR.funcLLVMVisibility = IR.LLVMVisDefault
                    , IR.funcMLIRVisibility = IR.MLIRVisPublic
                    , IR.funcBody = Just block
                    , IR.funcArgs = [] -- No arguments for REPL expression wrapper
                    , IR.funcDbgArgs = []
                    , IR.funcLoc = Nothing
                    , IR.funcResult = resultType
                    , IR.funcSymbol = symbol
                    }

        -- Add the function to the module
        State.modify $ \s ->
            s
                { currentModule =
                    (currentModule s)
                        { IR.moduleFunctions = wrapperFunc : IR.moduleFunctions (currentModule s)
                        }
                }

    -- Emit the module to MLIR text
    IR.emitModuleToText (currentModule loweringState')

toEffLogLevel :: B.LogLevel -> LogLevel
toEffLogLevel B.LogError = LogAttention
toEffLogLevel B.LogWarning = LogAttention
toEffLogLevel B.LogInfo = LogInfo
toEffLogLevel B.LogDebug = LogTrace
toEffLogLevel B.LogTrace = LogTrace

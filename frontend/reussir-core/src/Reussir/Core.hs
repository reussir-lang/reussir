{-# LANGUAGE RecordWildCards #-}

module Reussir.Core (
    translateProgToModule,
) where

import Control.Monad (forM, forM_)
import Data.Text qualified as T
import Effectful (Eff, IOE, inject, (:>))
import Effectful.Log qualified as L
import Effectful.Prim (runPrim)
import Effectful.Prim.IORef.Strict (Prim)
import Effectful.State.Static.Local (runState)
import Reussir.Codegen qualified as IR
import Reussir.Codegen.Context qualified as IR
import Reussir.Core.Data.Full.Context (FullContext (..))
import Reussir.Core.Data.Semi.Context (SemiContext (translationReports))
import Reussir.Core.Full.Conversion (convertCtx)
import Reussir.Core.Lowering.Context
import Reussir.Core.Lowering.Module (lowerModule)
import Reussir.Core.Semi.Context (emptySemiContext, populateRecordFields, scanStmt)
import Reussir.Core.Semi.FlowAnalysis (solveAllGenerics)
import Reussir.Core.Semi.Tyck (checkFuncType)
import Reussir.Diagnostic.Display (displayReport)
import Reussir.Diagnostic.Repository (createRepository)
import Reussir.Parser.Prog qualified as Syn
import Reussir.Parser.Types.Lexer (WithSpan (..))
import Reussir.Parser.Types.Stmt qualified as Syn
import System.IO (stderr)

unspanStmt :: Syn.Stmt -> Syn.Stmt
unspanStmt (Syn.SpannedStmt (WithSpan s _ _)) = unspanStmt s
unspanStmt stmt = stmt

translateProgToModule ::
    (IOE :> es, Prim :> es, L.Log :> es) => IR.TargetSpec -> Syn.Prog -> Eff es (Maybe IR.Module)
translateProgToModule spec prog = do
    let filePath = IR.moduleFilePath spec
    L.logTrace_ $ T.pack "translateProgToModule: scanning statements for " <> T.pack filePath
    repository <- createRepository [filePath]
    (elaborated, finalSemiCtx) <- do
        -- 1. Semi Elab
        initSemiState <- runPrim $ emptySemiContext (IR.logLevel spec) filePath
        (slns, finalSemiState) <- runPrim $ runState initSemiState $ do
            -- Scan all statements
            forM_ prog $ \stmt -> inject $ scanStmt stmt

            -- Populate record fields
            forM_ prog $ \stmt -> inject $ populateRecordFields stmt

            -- Elaborate all functions
            forM_ prog $ \stmt -> do
                case unspanStmt stmt of
                    Syn.FunctionStmt f -> do
                        _ <- inject $ checkFuncType f
                        return ()
                    _ -> return ()

            -- Solve generics
            inject solveAllGenerics

        case slns of
            Nothing -> return (Nothing, finalSemiState)
            Just solutions -> do
                -- 2. Full Conversion
                fullCtx <- runPrim $ convertCtx finalSemiState solutions
                return (Just fullCtx, finalSemiState)

    -- Report errors from Semi Elab
    forM_ (translationReports finalSemiCtx) $ \report -> do
        displayReport report repository 0 stderr

    forM elaborated $ \FullContext{..} -> do
        L.logTrace_ $ T.pack "translateProgToModule: lowering module for " <> T.pack filePath
        loweringCtx <-
            inject $
                createLoweringContext
                    repository
                    ctxFunctions
                    ctxRecords
                    ctxStringUniqifier
                    spec
        runLoweringToModule loweringCtx lowerModule

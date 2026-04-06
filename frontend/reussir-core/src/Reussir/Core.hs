{-# LANGUAGE RecordWildCards #-}

module Reussir.Core (
    translateProgToModule,
    translatePackageToModule,
) where

import Control.Monad (forM, forM_)
import Effectful (Eff, IOE, inject, (:>))
import Effectful.Prim (runPrim)
import Effectful.Prim.IORef.Strict (Prim)
import Effectful.State.Static.Local (runState)
import Reussir.Diagnostic.Display (displayReport)
import Reussir.Diagnostic.Repository (createRepository)
import Reussir.Parser.Types.Lexer (Identifier, WithSpan (..))
import System.IO (stderr)

import Data.Text qualified as T
import Effectful.Log qualified as L
import Reussir.Codegen qualified as IR
import Reussir.Codegen.Context qualified as IR
import Reussir.Parser.Prog qualified as Syn
import Reussir.Parser.Types.Stmt qualified as Syn

import Reussir.Core.Data.Full.Context (FullContext (..))
import Reussir.Core.Data.Semi.Context (SemiContext (translationReports))
import Reussir.Core.Full.Conversion (convertCtx)
import Reussir.Core.Lowering.Context
import Reussir.Core.Lowering.Module (lowerModule)
import Reussir.Core.Semi.Context (
    emptySemiContext,
    populateRecordFields,
    scanStmt,
    withModuleFile,
 )
import Reussir.Core.Semi.FlowAnalysis (solveAllGenerics)
import Reussir.Core.Semi.Trampoline (resolveTrampoline)
import Reussir.Core.Semi.Tyck (checkFuncType)

unspanStmt :: Syn.Stmt -> Syn.Stmt
unspanStmt (Syn.SpannedStmt (WithSpan s _ _)) = unspanStmt s
unspanStmt stmt = stmt

-- | Compile a single source file to an IR module (backward compatible entry point).
translateProgToModule ::
    (IOE :> es, Prim :> es, L.Log :> es) =>
    IR.TargetSpec -> Syn.Prog -> Eff es (Maybe IR.Module)
translateProgToModule spec prog =
    translatePackageToModule spec [(IR.moduleFilePath spec, [], prog)]

{- | Compile multiple source files (a package) to a single IR module.

Each entry in the list is a @(filePath, modulePath, parsedProgram)@ triple.
The @modulePath@ is the list of module segments for that file (e.g.,
@[\"mylib\", \"utils\"]@ for @mylib::utils@). For single-file mode, pass
an empty module path.

All files are scanned into a shared symbol table, then type-checked together,
allowing cross-file references to resolve naturally.
-}
translatePackageToModule ::
    (IOE :> es, Prim :> es, L.Log :> es) =>
    IR.TargetSpec ->
    [(FilePath, [Identifier], Syn.Prog)] ->
    Eff es (Maybe IR.Module)
translatePackageToModule spec files = do
    let allFilePaths = map (\(fp, _, _) -> fp) files
    let primaryFilePath = case files of
            ((fp, _, _) : _) -> fp
            [] -> IR.moduleFilePath spec
    let primaryModPath = case files of
            ((_, mp, _) : _) -> mp
            [] -> []

    L.logTrace_ $
        T.pack ("translatePackageToModule: scanning "
            <> show (length files)
            <> " file(s)")

    repository <- createRepository allFilePaths
    (elaborated, finalSemiCtx) <- do
        -- 1. Semi Elab
        initSemiState <- runPrim $ emptySemiContext (IR.logLevel spec) primaryFilePath primaryModPath
        (slns, finalSemiState) <- runPrim $ runState initSemiState $ do
            -- Scan all statements from all files
            forM_ files $ \(fp, modPath, prog) ->
                inject $ withModuleFile fp modPath $
                    forM_ prog $ \stmt -> inject $ scanStmt stmt

            -- Populate record fields from all files
            forM_ files $ \(fp, modPath, prog) ->
                inject $ withModuleFile fp modPath $
                    forM_ prog $ \stmt -> inject $ populateRecordFields stmt

            -- Elaborate all functions from all files
            forM_ files $ \(fp, modPath, prog) ->
                inject $ withModuleFile fp modPath $
                    forM_ prog $ \stmt -> do
                        case unspanStmt stmt of
                            Syn.FunctionStmt f -> do
                                _ <- inject $ checkFuncType f
                                return ()
                            Syn.ExternTrampolineStmt name abi target args -> do
                                inject $ resolveTrampoline name abi target args
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
        L.logTrace_ $
            T.pack "translatePackageToModule: lowering module"
        loweringCtx <-
            inject $
                createLoweringContext
                    repository
                    ctxFunctions
                    ctxRecords
                    ctxStringUniqifier
                    ctxTrampolines
                    spec
        runLoweringToModule loweringCtx lowerModule

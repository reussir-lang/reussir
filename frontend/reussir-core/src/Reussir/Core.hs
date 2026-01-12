{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core where

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import Effectful (Eff, IOE, inject, liftIO, (:>))
import Effectful.Log qualified as L
import Effectful.Prim (Prim)
import Effectful.State.Static.Local (execState, runState)
import GHC.IO.Handle.FD (stderr)
import Reussir.Codegen qualified as IR
import Reussir.Codegen.Context qualified as IR
import Reussir.Core.Lowering (createLoweringState, translateModule)
import Reussir.Core.Translation (emptyTranslationState, scanStmt, solveAllGenerics)
import Reussir.Core.Tyck (checkFuncType)
import Reussir.Core.Types.Lowering (currentModule)
import Reussir.Core.Types.Translation (TranslationState (translationReports))
import Reussir.Diagnostic.Display (displayReport)
import Reussir.Diagnostic.Repository (createRepository)
import Reussir.Parser.Prog qualified as Syn
import Reussir.Parser.Types.Lexer (WithSpan (spanValue))
import Reussir.Parser.Types.Stmt qualified as Syn

translateProgToModule ::
    (IOE :> es, Prim :> es, L.Log :> es) => FilePath -> IR.TargetSpec -> Syn.Prog -> Eff es IR.Module
translateProgToModule filePath spec prog = do
    L.logTrace_ $ T.pack "translateProgToModule: scanning statements for " <> T.pack filePath
    repository <- createRepository [filePath]
    translationState <- emptyTranslationState (IR.logLevel spec) filePath
    -- first scanAllStmt to build index
    state <- execState translationState $ inject $ do
        mapM_ scanStmt prog
    -- now translate all functions
    (genericSolutions, state') <- runState state $ do
        L.logTrace_ (T.pack "translateProgToModule: type checking functions")
        forM_ prog $ \stmt -> case stripSpan stmt of
            Syn.FunctionStmt f -> do
                _ <- inject $ checkFuncType f
                return ()
            _ -> return ()
        L.logTrace_ (T.pack "translateProgToModule: solving generics")
        inject solveAllGenerics

    -- report all diagnostics
    forM_ (translationReports state') $ \report -> do
        displayReport report repository 0 stderr
        liftIO $ hPutStrLn stderr ""

    let emptyMod = IR.emptyModule spec
    case genericSolutions of
        Nothing -> return emptyMod
        Just solutions -> do
            -- Lowering to IR.Module
            L.logTrace_ (T.pack "translateProgToModule: lowering to IR")
            loweringState <- createLoweringState filePath repository emptyMod state'
            loweringState' <- execState loweringState $ inject $ translateModule solutions
            return (currentModule loweringState')
  where
    stripSpan :: Syn.Stmt -> Syn.Stmt
    stripSpan (Syn.SpannedStmt s) = stripSpan (spanValue s)
    stripSpan stmt = stmt

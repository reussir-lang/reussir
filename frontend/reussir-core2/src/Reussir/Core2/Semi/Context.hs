{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core2.Semi.Context (
    withSpan,
    withMaybeSpan,
    withVariable,
    runUnification,
    addErrReport,
    addErrReportMsg,
) where

import Data.Function ((&))
import Data.Int (Int64)
import Data.Text qualified as T
import Effectful (inject)
import Effectful.Log qualified as L
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local qualified as State
import Reussir.Core2.Semi.Variable (newVariable, rollbackVar)
import Reussir.Core2.Types (SemiEff)
import Reussir.Core2.Types.Semi (SemiContext (..))
import Reussir.Core2.Types.Semi qualified as Semi
import Reussir.Core2.Types.Semi.Unification (UnificationEff)
import Reussir.Core2.Types.UniqueID (VarID)
import Reussir.Diagnostic (Label (..))
import Reussir.Diagnostic.Report (Report (..), addForegroundColorToCodeRef, addForegroundColorToText, annotatedCodeRef, defaultCodeRef, defaultText)
import Reussir.Parser.Types.Lexer (Identifier)
import System.Console.ANSI.Types qualified as ANSI

withSpan :: (Int64, Int64) -> SemiEff a -> SemiEff a
withSpan span' cont = do
    oldSpan <- State.gets currentSpan
    State.modify $ \s -> s{currentSpan = Just span'}
    result <- cont
    State.modify $ \s -> s{currentSpan = oldSpan}
    return result

withMaybeSpan :: Maybe (Int64, Int64) -> SemiEff a -> SemiEff a
withMaybeSpan Nothing cont = cont
withMaybeSpan (Just span') cont = withSpan span' cont

withVariable ::
    Identifier ->
    Maybe (Int64, Int64) ->
    Semi.Type ->
    (VarID -> SemiEff a) ->
    SemiEff a
withVariable varName varSpan varType cont = do
    vt <- State.gets varTable
    (varID, changeLog) <- newVariable varName varSpan varType vt
    result <- cont varID
    rollbackVar changeLog vt
    pure result

runUnification :: UnificationEff a -> SemiEff a
runUnification eff = do
    holeTable <- State.gets holeTable
    classDAG <- State.gets typeClassDAG
    typeClassTable <- State.gets typeClassTable
    genericState <- State.gets generics
    runReader holeTable $
        runReader classDAG $
            runReader typeClassTable $
                runReader genericState $
                    inject eff

addErrReport :: Report -> SemiEff ()
addErrReport report = do
    State.modify $ \st ->
        st{translationReports = report : translationReports st, translationHasFailed = True}

addErrReportMsg :: T.Text -> SemiEff ()
addErrReportMsg msg = do
    st <- State.get
    let span' = currentSpan st
    L.logTrace_ $ "reporting error at span: " <> T.pack (show span')
    let file = currentFile st
    let report = case span' of
            Just (start, end) ->
                let cr =
                        defaultCodeRef file start end
                            & addForegroundColorToCodeRef ANSI.Red ANSI.Vivid
                    msgText =
                        defaultText msg
                            & addForegroundColorToText ANSI.Red ANSI.Vivid
                 in Labeled Error (FormattedText [defaultText "Type Error"])
                        <> Nested (annotatedCodeRef cr msgText)
            Nothing ->
                Labeled Error (FormattedText [defaultText msg])
    addErrReport report

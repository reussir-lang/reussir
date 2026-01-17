{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core2.Semi.Context (
    withSpan,
    withMaybeSpan,
    withVariable,
    runUnification,
    addErrReport,
    addErrReportMsg,
) where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.Text qualified as T
import Effectful (inject, liftIO)
import Effectful.Log qualified as L
import Effectful.Prim.IORef.Strict (newIORef')
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local qualified as State
import Reussir.Core2.Class (addClass, populateDAG)
import Reussir.Core2.Data (SemiEff)
import Reussir.Core2.Data.Class (Class (Class))
import Reussir.Core2.Data.FP (FloatingPointType (..))
import Reussir.Core2.Data.Function (FunctionProto (..), FunctionTable (..))
import Reussir.Core2.Data.Integral (IntegralType (..))
import Reussir.Core2.Data.Semi (SemiContext (..), Type (..))
import Reussir.Core2.Data.Semi qualified as Semi
import Reussir.Core2.Data.Semi.Unification (UnificationEff)
import Reussir.Core2.Data.UniqueID (GenericID (..), VarID)
import Reussir.Core2.Generic (newGenericVar)
import Reussir.Core2.Semi.Type (addClassToType, substituteGeneric)
import Reussir.Core2.Semi.Variable (newVariable, rollbackVar)
import Reussir.Diagnostic (Label (..))
import Reussir.Diagnostic.Report (Report (..), addForegroundColorToCodeRef, addForegroundColorToText, annotatedCodeRef, defaultCodeRef, defaultText)
import Reussir.Parser.Types.Lexer (Identifier (..), Path (Path))
import Reussir.Parser.Types.Stmt qualified as Syn
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

populatePrimitives :: SemiEff ()
populatePrimitives = do
    typeClassTable <- State.gets typeClassTable
    typeClassDAG <- State.gets typeClassDAG
    let numClass = Class $ Path "Num" []
    let floatClass = Class $ Path "FloatingPoint" []
    let intClass = Class $ Path "Integral" []
    let ptrLikeClass = Class $ Path "PtrLike" []

    addClass ptrLikeClass [] typeClassDAG
    addClass numClass [] typeClassDAG
    addClass floatClass [numClass] typeClassDAG
    addClass intClass [numClass] typeClassDAG
    populateDAG typeClassDAG

    let fpTypes =
            [ IEEEFloat 16
            , IEEEFloat 32
            , IEEEFloat 64
            , BFloat16
            , Float8
            ]
    forM_ fpTypes $ \fp ->
        addClassToType typeClassTable (TypeFP fp) floatClass

    let intTypes =
            [ Signed 8
            , Signed 16
            , Signed 32
            , Signed 64
            , Unsigned 8
            , Unsigned 16
            , Unsigned 32
            , Unsigned 64
            ]
    forM_ intTypes $ \it ->
        addClassToType typeClassTable (TypeIntegral it) intClass

populateIntrinsics :: SemiEff ()
populateIntrinsics = do
    functionTable <- State.gets functions
    genericState <- State.gets generics
    let floatBound = [Path "FloatingPoint" []]
    let intBound = [Path "Integral" []]

    let addFunc namespace name bounds params retTy = do
            gid <- newGenericVar "T" Nothing bounds genericState
            let generics = [("T", gid)]
            let paramTys = map (\t -> substituteGeneric t (\g -> if g == GenericID 0 then Just (TypeGeneric gid) else Nothing)) params
            let retTy' = substituteGeneric retTy (\g -> if g == GenericID 0 then Just (TypeGeneric gid) else Nothing)
            pendingBody <- newIORef' Nothing
            let proto =
                    FunctionProto
                        { funcVisibility = Syn.Public
                        , funcName = name
                        , funcGenerics = generics
                        , funcParams = zipWith (\i t -> (Identifier (T.pack $ "arg" <> show i), t)) [0 :: Int ..] paramTys
                        , funcReturnType = retTy'
                        , funcIsRegional = False
                        , funcBody = pendingBody
                        , funcSpan = Nothing
                        }
            let path = Path name namespace
            liftIO $ H.insert (functionProtos functionTable) path proto
    let addMathFunc = addFunc ["core", "intrinsic", "math"]
    let t = TypeGeneric (GenericID 0)
    let u32 = TypeIntegral (Unsigned 32)
    -- Float Unary
    let floatUnary =
            [ "absf"
            , "acos"
            , "acosh"
            , "asin"
            , "asinh"
            , "atan"
            , "atanh"
            , "cbrt"
            , "ceil"
            , "cos"
            , "cosh"
            , "erf"
            , "erfc"
            , "exp"
            , "exp2"
            , "expm1"
            , "floor"
            , "log10"
            , "log1p"
            , "log2"
            , "round"
            , "roundeven"
            , "rsqrt"
            , "sin"
            , "sinh"
            , "sqrt"
            , "tan"
            , "tanh"
            , "trunc"
            ]
    forM_ floatUnary $ \name -> addMathFunc name floatBound [t, u32] t

    let checks = ["isfinite", "isinf", "isnan", "isnormal"]
    forM_ checks $ \name -> addMathFunc name floatBound [t, u32] TypeBool

    -- Float Binary
    let floatBinary = ["atan2", "copysign", "powf"]
    forM_ floatBinary $ \name -> addMathFunc name floatBound [t, t, u32] t

    -- Float Ternary
    addMathFunc "fma" floatBound [t, t, t, u32] t

    -- Float Special
    addMathFunc "fpowi" floatBound [t, TypeIntegral (Signed 32), u32] t

    -- Int Unary
    let intUnary = ["absi", "ctlz", "ctpop", "cttz"]
    forM_ intUnary $ \name -> addMathFunc name intBound [t] t

    -- Int Binary
    addMathFunc "ipowi" intBound [t, t] t

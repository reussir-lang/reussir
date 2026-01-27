{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Semi.Context (
    withSpan,
    withMaybeSpan,
    runUnification,
    addErrReport,
    addErrReportMsg,
    emptySemiContext,
    emptyLocalSemiContext,
    evalType,
    evalTypeWithFlexivity,
    scanProg,
    scanStmt,
    populateRecordFields,
    exprWithSpan,
    withFreshLocalContext,
    withGenericContext,
    addErrReportMsgSeq,
    withVariable,
) where

import Control.Monad (forM_, unless)
import Data.Function ((&))
import Data.HashMap.Strict qualified as HashMap
import Reussir.Core.Uitls.HashTable qualified as HU
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Vector.Strict qualified as V
import Effectful (Eff, IOE, inject, (:>))
import Effectful.Log qualified as L
import Effectful.Prim.IORef (Prim)
import Effectful.Prim.IORef.Strict (newIORef', writeIORef')
import Effectful.Reader.Static (runReader)
import Effectful.State.Static.Local (runState)
import Effectful.State.Static.Local qualified as State
import Reussir.Bridge qualified as B
import Reussir.Core.Class (addClass, newDAG, populateDAG)
import Reussir.Core.Data.Class (Class (Class), ClassDAG)
import Reussir.Core.Data.FP (FloatingPointType (..))
import Reussir.Core.Data.Integral (IntegralType (..))
import Reussir.Core.Data.Semi.Context (
    GlobalSemiEff,
    LocalSemiContext (..),
    SemiContext (..),
    SemiEff,
 )
import Reussir.Core.Data.Semi.Expr
import Reussir.Core.Data.Semi.Function (FunctionProto (..), FunctionTable (..))
import Reussir.Core.Data.Semi.Record
import Reussir.Core.Data.Semi.Type (Flexivity (..), Type (..), TypeClassTable)
import Reussir.Core.Data.Semi.Type qualified as Semi
import Reussir.Core.Data.Semi.Unification (UnificationEff)
import Reussir.Core.Data.String (StringUniqifier (StringUniqifier))
import Reussir.Core.Data.UniqueID (ExprID (..), GenericID (..), VarID)
import Reussir.Core.Generic (emptyGenericState, newGenericVar)
import Reussir.Core.Semi.Function (newFunctionTable)
import Reussir.Core.Semi.Type (addClassToType, emptyTypeClassTable)
import Reussir.Core.Semi.Unification (newHoleTable)
import Reussir.Core.Semi.Variable (newVariable, newVariableTable, rollbackVar)
import Reussir.Diagnostic (Label (..))
import Reussir.Diagnostic.Report (
    Report (..),
    addForegroundColorToCodeRef,
    addForegroundColorToText,
    annotatedCodeRef,
    defaultCodeRef,
    defaultText,
 )
import Reussir.Parser.Prog qualified as Syn
import Reussir.Parser.Types.Capability qualified as Syn
import Reussir.Parser.Types.Lexer (Identifier (..), Path (..), WithSpan (..))
import Reussir.Parser.Types.Stmt qualified as Syn
import Reussir.Parser.Types.Type qualified as Syn
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

addErrReport :: Report -> GlobalSemiEff ()
addErrReport report = do
    State.modify $ \st ->
        st{translationReports = report : translationReports st, translationHasFailed = True}

addErrReportMsgSeq :: T.Text -> Maybe Report -> SemiEff ()
addErrReportMsgSeq msg additonal = do
    span' <- State.gets currentSpan
    L.logTrace_ $ "reporting error at span: " <> T.pack (show span')
    file <- State.gets currentFile
    let report = case span' of
            Just (start, end) ->
                let cr =
                        defaultCodeRef file start end
                            & addForegroundColorToCodeRef ANSI.Red ANSI.Vivid
                    msgText =
                        defaultText msg
                            & addForegroundColorToText ANSI.Red ANSI.Vivid
                 in Labeled Error (FormattedText [defaultText "Elaboration Error"])
                        <> Nested (annotatedCodeRef cr msgText)
            Nothing ->
                Labeled Error (FormattedText [defaultText msg])
    let report' = case additonal of
            Just add' -> report <> add'
            Nothing -> report
    inject $ addErrReport report'

addErrReportMsg :: T.Text -> SemiEff ()
addErrReportMsg msg = addErrReportMsgSeq msg Nothing

populatePrimitives :: (IOE :> es, Prim :> es) => TypeClassTable -> ClassDAG -> Eff es ()
populatePrimitives typeClassTable typeClassDAG = do
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

-- | Initialize the semi context with the given log level and file path
emptySemiContext ::
    (IOE :> es, Prim :> es) => B.LogLevel -> FilePath -> Eff es SemiContext
emptySemiContext translationLogLevel currentFile = do
    table <- HU.new
    let stringUniqifier = StringUniqifier table
    typeClassDAG <- newDAG
    typeClassTable <- emptyTypeClassTable
    populatePrimitives typeClassTable typeClassDAG
    knownRecords <- HU.new
    functions <- newFunctionTable
    generics <- emptyGenericState
    return $
        SemiContext
            { currentFile
            , translationLogLevel
            , stringUniqifier
            , translationReports = []
            , typeClassDAG
            , typeClassTable
            , knownRecords
            , functions
            , generics
            , translationHasFailed = False
            }

-- | empty local context
emptyLocalSemiContext :: (IOE :> es, Prim :> es) => Eff es LocalSemiContext
emptyLocalSemiContext = do
    holeTable <- newHoleTable
    varTable <- newVariableTable
    let genericNameMap = HashMap.empty
    return $
        LocalSemiContext
            { currentSpan = Nothing
            , holeTable
            , varTable
            , genericNameMap
            , insideRegion = False
            , exprCounter = 0
            }

{- |
Execute a computation with a set of generic variables in scope.
This temporarily updates the 'genericNameMap' in the translation state.
If a generic name shadows an existing one, the old one is restored after the computation.
-}
withGenericContext :: [(Identifier, GenericID)] -> SemiEff a -> SemiEff a
withGenericContext newGenerics action = do
    oldMap <- State.gets genericNameMap
    let newMap = foldl' (\m (k, v) -> HashMap.insert k v m) oldMap newGenerics
    State.modify $ \st -> st{genericNameMap = newMap}
    result <- action
    State.modify $ \st -> st{genericNameMap = oldMap}
    return result

translateGeneric :: (Identifier, [Path]) -> SemiEff (Identifier, GenericID)
translateGeneric (identifier, bounds) = do
    st <- State.gets generics
    gid <- newGenericVar identifier Nothing bounds st -- TODO: handle span
    return (identifier, gid)

-- convert syntax type to semantic type without the consideration of memory modality
evalType :: Syn.Type -> SemiEff Type
evalType (Syn.TypeSpanned s) = evalType (spanValue s)
evalType (Syn.TypeExpr (Path "Nullable" []) args) = do
    arg <- case args of
        [arg] -> evalType arg
        _ -> do
            addErrReportMsg "Nullable type must have exactly one type argument"
            return TypeBottom
    return $ TypeNullable arg
evalType (Syn.TypeExpr path args) = do
    args' <- mapM evalType args

    -- Check if it's a generic variable
    mGeneric <- case path of
        Path name [] -> do
            genericMap <- State.gets genericNameMap
            return $ HashMap.lookup name genericMap
        _ -> return Nothing

    case mGeneric of
        Just gid -> do
            unless (null args) $ do
                addErrReportMsg $
                    "Generic type "
                        <> unIdentifier (pathBasename path)
                        <> " cannot have type arguments"
            return $ TypeGeneric gid
        Nothing -> do
            knownRecords <- State.gets knownRecords
            HU.lookup knownRecords path >>= \case
                Just record -> case recordDefaultCap record of
                    Syn.Value -> return $ TypeRecord path args' Irrelevant
                    Syn.Shared -> return $ TypeRecord path args' Irrelevant
                    Syn.Regional -> return $ TypeRecord path args' Regional
                    cap -> do
                        addErrReportMsg $ "Unsupported capability " <> T.pack (show cap) <> " for record " <> T.pack (show path)
                        return TypeBottom
                Nothing -> do
                    addErrReportMsg $ "Unknown type: " <> T.pack (show path)
                    return TypeBottom
evalType (Syn.TypeIntegral (Syn.Signed n)) = return $ TypeIntegral (Signed n)
evalType (Syn.TypeIntegral (Syn.Unsigned n)) = return $ TypeIntegral (Unsigned n)
evalType (Syn.TypeFP (Syn.IEEEFloat n)) = return $ TypeFP (IEEEFloat n)
evalType (Syn.TypeFP Syn.BFloat16) = return $ TypeFP BFloat16
evalType (Syn.TypeFP Syn.Float8) = return $ TypeFP Float8
evalType Syn.TypeBool = return TypeBool
evalType Syn.TypeStr = return TypeStr
evalType Syn.TypeUnit = return TypeUnit
evalType Syn.TypeBottom = return TypeBottom
evalType (Syn.TypeArrow t1 t2) = do
    t1' <- evalType t1
    (args, ret) <- unfoldArrow t2
    return $ TypeClosure (t1' : args) ret
  where
    unfoldArrow :: Syn.Type -> SemiEff ([Type], Type)
    unfoldArrow (Syn.TypeArrow a b) = do
        a' <- evalType a
        (args, ret) <- unfoldArrow b
        return (a' : args, ret)
    unfoldArrow (Syn.TypeSpanned s) = unfoldArrow (spanValue s)
    unfoldArrow t = do
        t' <- evalType t
        return ([], t')

-- | Convert a type to a type with explicit flexivity
evalTypeWithFlexivity :: Syn.Type -> Bool -> SemiEff Type
evalTypeWithFlexivity t isFlexible = do
    t' <- evalType t
    case t' of
        TypeRecord path args Regional -> do
            if isFlexible
                then return $ TypeRecord path args Flex
                else return $ TypeRecord path args Rigid
        TypeNullable (TypeRecord path args Regional) -> do
            if isFlexible
                then return $ TypeNullable (TypeRecord path args Flex)
                else return $ TypeNullable (TypeRecord path args Rigid)
        _ -> return t'

-- | Add a record definition to the known records table
addRecordDefinition :: Path -> Record -> SemiEff ()
addRecordDefinition path record = do
    records <- State.gets knownRecords
    HU.insert records path record

-- | The initial scan to build up the index of functions and records
scanStmt :: Syn.Stmt -> GlobalSemiEff ()
scanStmt stmt = withFreshLocalContext $ scanStmtImpl stmt

scanStmtImpl :: Syn.Stmt -> SemiEff ()
scanStmtImpl (Syn.SpannedStmt s) = do
    backup <- State.gets currentSpan
    State.modify $ \st -> st{currentSpan = Just (spanStartOffset s, spanEndOffset s)}
    scanStmtImpl (spanValue s)
    State.modify $ \st -> st{currentSpan = backup}
scanStmtImpl (Syn.RecordStmt record) = do
    let name = Syn.recordName record
    let tyParams = Syn.recordTyParams record
    -- Translate generics
    genericsList <- mapM translateGeneric tyParams

    recSpan <- State.gets currentSpan

    -- Create the main record with empty fields
    fieldsRef <- newIORef' Nothing

    let kind = case Syn.recordKind record of
            Syn.StructKind -> StructKind
            Syn.EnumKind -> EnumKind

    let semRecord =
            Record
                { recordName = Path name [] -- TODO: handle module path
                , recordTyParams = genericsList
                , recordFields = fieldsRef
                , recordKind = kind
                , recordVisibility = Syn.recordVisibility record
                , recordDefaultCap = Syn.recordDefaultCap record
                , recordSpan = recSpan
                }

    -- For variants, we add each sub-variant as a record
    case Syn.recordFields record of
        Syn.Variants vs -> do
            V.iforM_ vs $ \variantIdx (WithSpan (variantName, _) s e) -> do
                let variantPath = Path variantName [name] -- TODO: handle module path
                variantFieldsRef <- newIORef' Nothing
                let variantRecord =
                        Record
                            { recordName = variantPath
                            , recordTyParams = genericsList -- share same generics as parent
                            , recordFields = variantFieldsRef
                            , recordKind = EnumVariant{variantParent = Path name [], variantIdx}
                            , recordVisibility = Syn.recordVisibility record
                            , recordDefaultCap = Syn.Value
                            , recordSpan = Just (s, e)
                            }
                addRecordDefinition variantPath variantRecord
        _ -> return ()

    let path = Path name [] -- TODO: handle module path
    addRecordDefinition path semRecord
scanStmtImpl
    ( Syn.FunctionStmt
            Syn.Function
                { Syn.funcVisibility = funcVisibility
                , Syn.funcName = funcName
                , Syn.funcGenerics = funcGenerics
                , Syn.funcParams = funcParams
                , Syn.funcReturnType = funcReturnType
                , Syn.funcIsRegional = funcIsRegional
                }
        ) = do
        genericsList <- mapM translateGeneric funcGenerics
        withGenericContext genericsList $ do
            paramsList <- mapM translateParam funcParams
            funcReturnType' <-
                mapM
                    (\(ty, flex) -> evalTypeWithFlexivity ty flex)
                    funcReturnType
            let returnTy = case funcReturnType' of
                    Just ty -> ty
                    Nothing -> TypeUnit
            pendingFuncBody <- newIORef' Nothing
            funcSpan <- State.gets currentSpan
            let proto =
                    FunctionProto
                        { funcVisibility = funcVisibility
                        , funcName = funcName
                        , funcGenerics = genericsList
                        , funcParams = paramsList
                        , funcReturnType = returnTy
                        , funcIsRegional = funcIsRegional
                        , funcBody = pendingFuncBody
                        , funcSpan = funcSpan
                        }
            -- TODO: handle module path
            let funcPath = Path funcName []
            functionTable <- State.gets functions
            existed <-
                isJust <$> HU.lookup (functionProtos functionTable) funcPath
            if existed
                then
                    addErrReportMsg $
                        "Function already defined: " <> unIdentifier funcName
                else HU.insert (functionProtos functionTable) funcPath proto
      where
        translateParam ::
            (Identifier, Syn.Type, Syn.FlexFlag) -> SemiEff (Identifier, Type)
        translateParam (paramName, ty, flex) = do
            ty' <- evalTypeWithFlexivity ty flex
            return (paramName, ty')

scanProg :: Syn.Prog -> GlobalSemiEff ()
scanProg = flip forM_ scanStmt

populateRecordFields :: Syn.Stmt -> GlobalSemiEff ()
populateRecordFields stmt = withFreshLocalContext $ populateRecordFieldsImpl stmt

populateRecordFieldsImpl :: Syn.Stmt -> SemiEff ()
populateRecordFieldsImpl (Syn.SpannedStmt s) = do
    withSpan (spanStartOffset s, spanEndOffset s) $ populateRecordFieldsImpl (spanValue s)
populateRecordFieldsImpl (Syn.RecordStmt record) = do
    let name = Syn.recordName record
    let path = Path name [] -- TODO: handle module path
    knownRecords <- State.gets knownRecords
    mRecord <- HU.lookup knownRecords path

    case mRecord of
        Nothing -> addErrReportMsg $ "Internal error: record not found during field population: " <> unIdentifier name
        Just rec -> do
            let existingGenerics = recordTyParams rec

            withGenericContext existingGenerics $ do
                (fields, variants) <- case Syn.recordFields record of
                    Syn.Named fs -> do
                        fs' <- V.mapM (\(WithSpan (n, t, f) s e) -> (\(n', t', f') -> WithSpan (n', t', f') s e) <$> ((n,,f) <$> evalType t)) fs
                        return (Named fs', Nothing)
                    Syn.Unnamed fs -> do
                        fs' <- V.mapM (\(WithSpan (t, f) s e) -> (\(t', f') -> WithSpan (t', f') s e) <$> ((,f) <$> evalType t)) fs
                        return (Unnamed fs', Nothing)
                    Syn.Variants vs -> do
                        vs' <- V.mapM (\(WithSpan (n, ts) s e) -> (\(n', ts') -> WithSpan (n', ts') s e) <$> ((n,) <$> mapM evalType ts)) vs
                        let names = V.map (\(WithSpan (n, _) s e) -> WithSpan n s e) vs'
                        return (Variants names, Just vs')

                writeIORef' (recordFields rec) (Just fields)

                case variants of
                    Just vs -> do
                        V.forM_ vs $ \(WithSpan (variantName, variantFields) s e) -> do
                            let variantPath = Path variantName [name]
                            mVariantRecord <- HU.lookup knownRecords variantPath
                            case mVariantRecord of
                                Nothing -> addErrReportMsg $ "Internal error: variant record not found: " <> unIdentifier variantName
                                Just vRec -> do
                                    let fieldsWithSpan = V.map (\t -> WithSpan (t, False) s e) variantFields
                                    writeIORef' (recordFields vRec) (Just (Unnamed fieldsWithSpan))
                    Nothing -> return ()
populateRecordFieldsImpl _ = return ()

exprWithSpan :: Type -> ExprKind -> SemiEff Expr
exprWithSpan exprType exprKind = do
    exprSpan <- State.gets currentSpan
    exprID <- ExprID <$> State.gets exprCounter
    State.modify $ \st -> st{exprCounter = exprCounter st + 1}
    return $ Expr{exprKind, exprSpan, exprType, exprID}

withFreshLocalContext :: SemiEff a -> GlobalSemiEff a
withFreshLocalContext cont = do
    localCtx <- emptyLocalSemiContext
    (res, _) <- runState localCtx $ inject cont
    return res

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

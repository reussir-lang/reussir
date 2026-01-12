{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Translation where

import Control.Monad (forM_, unless, when, zipWithM, zipWithM_)
import Data.Digest.XXHash.FFI (XXH3 (XXH3))
import Data.Function ((&))
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable (hash))
import Data.Int (Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Log qualified as L
import Effectful.Prim (Prim)
import Effectful.Prim.IORef.Strict (IORef', newIORef', readIORef', writeIORef')
import Effectful.State.Static.Local qualified as State
import Reussir.Bridge qualified as B
import Reussir.Core.Class (
    addClass,
    isSuperClass,
    meetBound,
    newDAG,
    populateDAG,
    subsumeBound,
 )
import Reussir.Core.Function (newFunctionTable)
import Reussir.Core.Generic (
    addConcreteFlow,
    addCtorLink,
    addDirectLink,
    emptyGenericState,
    newGenericVar,
    solveGeneric,
 )
import Reussir.Core.Type qualified as Sem
import Reussir.Core.Types.Class (Class (..), ClassDAG, TypeBound)
import Reussir.Core.Types.Expr qualified as Sem
import Reussir.Core.Types.Function qualified as Sem
import Reussir.Core.Types.Generic (
    GenericState (getStateRef),
    GenericVar (genericBounds),
 )
import Reussir.Core.Types.GenericID (GenericID (..))
import Reussir.Core.Types.Record qualified as Sem
import Reussir.Core.Types.String (StringToken, StringUniqifier (..))
import Reussir.Core.Types.Translation
import Reussir.Core.Types.Type qualified as Sem
import Reussir.Diagnostic (Label (Error), Report (..))
import Reussir.Diagnostic.Report (
    addForegroundColorToCodeRef,
    addForegroundColorToText,
    annotatedCodeRef,
    defaultCodeRef,
    defaultText,
 )
import Reussir.Parser.Prog qualified as Syn
import Reussir.Parser.Types.Capability (Capability (..))
import Reussir.Parser.Types.Lexer (
    Identifier,
    Path (..),
    WithSpan (..),
    pathBasename,
    unIdentifier,
 )
import Reussir.Parser.Types.Stmt qualified as Syn
import Reussir.Parser.Types.Type qualified as Syn
import System.Console.ANSI.Types qualified as ANSI

type Tyck = Eff '[IOE, Prim, L.Log, State.State TranslationState]

clearLocals :: Tyck ()
clearLocals = do
    clearHoles
    emptyMap <- liftIO H.new
    State.modify $ \s -> s{variableStates = mempty, variableNameMap = emptyMap}

strToToken :: (IOE :> es) => T.Text -> StringUniqifier -> Eff es StringToken
strToToken str (StringUniqifier table) = do
    let h = hash (XXH3 str)
    bucket <- liftIO $ H.lookup table h
    let seqs = fromMaybe Seq.empty bucket
    case Seq.elemIndexL str seqs of
        Just idx -> return (h, fromIntegral idx)
        Nothing -> do
            let newSeqs = seqs Seq.|> str
            liftIO $ H.insert table h newSeqs
            return (h, fromIntegral (Seq.length newSeqs - 1))

withVariable ::
    Identifier ->
    Maybe (Int64, Int64) ->
    Sem.Type ->
    (Sem.VarID -> Tyck a) ->
    Tyck a
withVariable varName varSpan varType cont = do
    varMap <- State.gets variableNameMap
    backup <- liftIO $ H.lookup varMap varName
    varID <- newVariable varName varSpan varType
    result <- cont varID
    case backup of
        Just oldVarID -> liftIO $ H.insert varMap varName oldVarID
        Nothing -> liftIO $ H.delete varMap varName
    pure result

newVariable :: Identifier -> Maybe (Int64, Int64) -> Sem.Type -> Tyck Sem.VarID
newVariable varName varSpan varType = do
    st <- State.gets variableStates
    let varID = Sem.VarID $ fromIntegral $ Seq.length st
    let varDef = VarDef{varName, varSpan, varType}
    State.modify $ \s ->
        s
            { variableStates = st Seq.|> varDef
            }
    nameMap <- State.gets variableNameMap
    liftIO $ H.insert nameMap varName varID
    return varID

getVarType :: Sem.VarID -> Tyck Sem.Type
getVarType (Sem.VarID idx) = do
    vars <- State.gets variableStates
    let varDef = Seq.index vars (fromIntegral idx)
    return $ varType varDef

lookupVar :: Identifier -> Tyck (Sem.VarID, Sem.Type)
lookupVar varName = do
    nameMap <- State.gets variableNameMap
    liftIO (H.lookup nameMap varName) >>= \case
        Just varID -> getVarType varID >>= \ty -> pure (varID, ty)
        Nothing -> do
            reportError $ "Variable not found: " <> (unIdentifier varName)
            return (Sem.VarID (-1), Sem.TypeBottom)

-- Introduce a new hole into the translation state
introduceNewHole ::
    Maybe T.Text ->
    Maybe (Int64, Int64) ->
    TypeBound ->
    Tyck Sem.Type
introduceNewHole holeName holeSpan bound = do
    st <- State.gets holes
    let holeID = fromIntegral $ Seq.length st
    holeUnification <- newIORef' $ UnSolvedUFRoot holeID bound
    let holeState = HoleState{holeName, holeSpan, holeUnification}
    State.modify $ \s -> s{holes = st Seq.|> holeState}
    return $ Sem.TypeHole $ Sem.HoleID holeID

-- Clear all holes from the translation state, this is used for process a new
-- function
clearHoles :: Tyck ()
clearHoles = do
    State.modify $ \s -> s{holes = mempty}

-- Find unification state of a hole
findHoleUnifState :: Sem.HoleID -> Tyck (Sem.HoleID, IORef' UnificationState)
findHoleUnifState hID@(Sem.HoleID idx) = do
    holesSeq <- State.gets holes
    let holeState = Seq.index holesSeq idx
    unifState <- readIORef' (holeUnification holeState)
    case unifState of
        UnSolvedUFRoot{} -> return (hID, holeUnification holeState)
        SolvedUFRoot{} -> return (hID, holeUnification holeState)
        UFNode parentID -> do
            (rootID, rootState) <- findHoleUnifState parentID
            when (parentID /= rootID) $ do
                writeIORef' (holeUnification holeState) (UFNode rootID)
            return (rootID, rootState)

-- This function list all variant on purpose to make it easier to add changes later on
force :: Sem.Type -> Tyck Sem.Type
force (Sem.TypeHole holeID) = do
    (newId, unifState) <- findHoleUnifState holeID
    unifState' <- readIORef' unifState
    case unifState' of
        SolvedUFRoot rk ty -> do
            ty' <- force ty
            writeIORef' unifState (SolvedUFRoot rk ty')
            return ty'
        UnSolvedUFRoot{} -> return $ Sem.TypeHole newId
        UFNode{} -> error "unreachable: findHoleUnifState should have returned root"
force (Sem.TypeRecord path args) = do
    args' <- mapM force args
    return $ Sem.TypeRecord path args'
force (Sem.TypeClosure args ret) =
    Sem.TypeClosure <$> mapM force args <*> force ret
force (Sem.TypeRc t cap) = Sem.TypeRc <$> force t <*> pure cap
force (Sem.TypeRef t cap) = Sem.TypeRef <$> force t <*> pure cap
force Sem.TypeBottom = return Sem.TypeBottom
force g@(Sem.TypeGeneric _) = return g
force Sem.TypeUnit = return Sem.TypeUnit
force Sem.TypeStr = return Sem.TypeStr
force Sem.TypeBool = return Sem.TypeBool
force int@(Sem.TypeIntegral _) = return int
force fp@(Sem.TypeFP _) = return fp

rnkOfUnifState :: UnificationState -> Int
rnkOfUnifState (UnSolvedUFRoot rnk _) = rnk
rnkOfUnifState (SolvedUFRoot rnk _) = rnk
rnkOfUnifState (UFNode _) = error "unreachable: UFNode has no rank"

-- This is used after force, so both holes are unsolved
unifyTwoHoles :: Sem.HoleID -> Sem.HoleID -> Tyck ()
unifyTwoHoles hID1 hID2 = do
    (rootID1, unifState1) <- findHoleUnifState hID1
    (rootID2, unifState2) <- findHoleUnifState hID2
    unifState1' <- readIORef' unifState1
    unifState2' <- readIORef' unifState2
    let unifRnk1 = rnkOfUnifState unifState1'
    let unifRnk2 = rnkOfUnifState unifState2'
    let ( minRnkState
            , minRnkStateRef
            , minRnk
            , maxRnkID
            , maxRnkState
            , maxRnkStateRef
            , maxRnk
            ) =
                if unifRnk1 <= unifRnk2
                    then
                        (unifState1', unifState1, unifRnk1, rootID2, unifState2', unifState2, unifRnk2)
                    else
                        (unifState2', unifState2, unifRnk2, rootID1, unifState1', unifState1, unifRnk1)
    case (minRnkState, maxRnkState) of
        (UnSolvedUFRoot _ bnds, UnSolvedUFRoot _ bnds') -> do
            let newRnk =
                    if minRnk == maxRnk
                        then minRnk + 1
                        else minRnk
            dag <- State.gets typeClassDAG
            newBound <- meetBound dag bnds bnds'
            writeIORef' maxRnkStateRef (UnSolvedUFRoot newRnk newBound)
            writeIORef' minRnkStateRef (UFNode maxRnkID)
        _ -> error "unreachable: unifyTwoHoles called on solved holes"

exactTypeSatisfyBounds :: Sem.TypeClassTable -> Sem.Type -> [Class] -> Tyck Bool
exactTypeSatisfyBounds tyClassTable ty bounds = do
    dag <- State.gets typeClassDAG
    tyClasses <- Sem.getClassesOfType tyClassTable ty
    let candidates = HashSet.toList tyClasses

    let checkBound b = do
            -- Check if any candidate 'c' satisfies 'isSuperClass dag b c'
            -- isSuperClass returns Eff es Bool
            results <- mapM (isSuperClass dag b) candidates
            return $ or results

    boundChecks <- mapM checkBound bounds
    return $ and boundChecks

-- unification
unify :: Sem.Type -> Sem.Type -> Tyck Bool
unify ty1 ty2 = do
    ty1' <- force ty1
    ty2' <- force ty2
    unifyForced ty1' ty2'
  where
    unifyForced (Sem.TypeHole hID1) (Sem.TypeHole hID2) = do
        unifyTwoHoles hID1 hID2
        -- TODO: may be we should detect some trivial error here. e.g.
        -- Integral and FloatingPoint bounds cannot be satisfied in the same time
        pure True
    unifyForced (Sem.TypeHole hID) ty = do
        (_, unifState) <- findHoleUnifState hID
        unifState' <- readIORef' unifState
        case unifState' of
            UnSolvedUFRoot rnk bnds -> do
                -- check if ty satisfies bounds
                -- TODO: for now, we simply check if type has Class, this is not enough
                -- and should be delayed
                isSatisfy <- satisfyBounds ty bnds
                when isSatisfy $
                    writeIORef' unifState (SolvedUFRoot rnk ty)
                return isSatisfy
            _ -> error "unreachable: cannot be solved or non-root here"
    unifyForced ty (Sem.TypeHole hID) = unifyForced (Sem.TypeHole hID) ty
    unifyForced (Sem.TypeRecord path1 args1) (Sem.TypeRecord path2 args2)
        | path1 == path2 && length args1 == length args2 = do
            results <- zipWithM unify args1 args2
            return $ and results
        | otherwise = return False
    unifyForced Sem.TypeBool Sem.TypeBool = return True
    unifyForced Sem.TypeStr Sem.TypeStr = return True
    unifyForced Sem.TypeUnit Sem.TypeUnit = return True
    unifyForced (Sem.TypeIntegral it1) (Sem.TypeIntegral it2) = return (it1 == it2)
    unifyForced (Sem.TypeFP fpt1) (Sem.TypeFP fpt2) = return (fpt1 == fpt2)
    -- TODO: covariance/contravariance?
    unifyForced (Sem.TypeClosure args1 ret1) (Sem.TypeClosure args2 ret2)
        | length args1 == length args2 = do
            argsResults <- zipWithM unify args1 args2
            retResult <- unify ret1 ret2
            return $ and (retResult : argsResults)
        | otherwise = return False
    unifyForced (Sem.TypeRc t1 cap1) (Sem.TypeRc t2 cap2)
        | cap1 == cap2 = unify t1 t2
        | otherwise = return False
    unifyForced (Sem.TypeRef t1 cap1) (Sem.TypeRef t2 cap2)
        | cap1 == cap2 = unify t1 t2
        | otherwise = return False
    -- auto coercion from bottom
    unifyForced Sem.TypeBottom _ = return True
    unifyForced _ Sem.TypeBottom = return True
    unifyForced (Sem.TypeGeneric g1) (Sem.TypeGeneric g2) = return (g1 == g2)
    unifyForced _ _ = return False

satisfyBounds :: Sem.Type -> TypeBound -> Tyck Bool
satisfyBounds ty bnds = do
    tyClassTable <- State.gets typeClassTable
    case ty of
        Sem.TypeHole hID -> do
            (_, unifState) <- findHoleUnifState hID
            unifState' <- readIORef' unifState
            case unifState' of
                UnSolvedUFRoot _ bnds' -> do
                    dag <- State.gets typeClassDAG
                    subsumeBound dag bnds' bnds
                SolvedUFRoot _ tySolved -> do
                    forced <- force tySolved
                    satisfyBounds forced bnds
                UFNode{} -> error "unreachable: cannot be non-root here"
        Sem.TypeGeneric gID -> do
            bounds <- getGenericBound gID
            dag <- State.gets typeClassDAG
            subsumeBound dag bounds bnds
        x -> exactTypeSatisfyBounds tyClassTable x bnds

populatePrimitives ::
    (IOE :> es, Prim :> es) => Sem.TypeClassTable -> ClassDAG -> Eff es ()
populatePrimitives typeClassTable typeClassDAG = do
    let numClass = Class $ Path "Num" []
    let floatClass = Class $ Path "FloatingPoint" []
    let intClass = Class $ Path "Integral" []
    addClass numClass [] typeClassDAG
    addClass floatClass [numClass] typeClassDAG
    addClass intClass [numClass] typeClassDAG
    populateDAG typeClassDAG

    let fpTypes =
            [ Sem.IEEEFloat 16
            , Sem.IEEEFloat 32
            , Sem.IEEEFloat 64
            , Sem.BFloat16
            , Sem.Float8
            ]
    forM_ fpTypes $ \fp ->
        Sem.addClassToType typeClassTable (Sem.TypeFP fp) floatClass

    let intTypes =
            [ Sem.Signed 8
            , Sem.Signed 16
            , Sem.Signed 32
            , Sem.Signed 64
            , Sem.Unsigned 8
            , Sem.Unsigned 16
            , Sem.Unsigned 32
            , Sem.Unsigned 64
            ]
    forM_ intTypes $ \it ->
        Sem.addClassToType typeClassTable (Sem.TypeIntegral it) intClass

emptyTranslationState ::
    (IOE :> es, Prim :> es) => B.LogLevel -> FilePath -> Eff es TranslationState
emptyTranslationState translationLogLevel currentFile = do
    table <- liftIO $ H.new
    variableNameMap <- liftIO $ H.new
    let genericNameMap = HashMap.empty
    let stringUniqifier = StringUniqifier table
    typeClassDAG <- newDAG
    typeClassTable <- Sem.emptyTypeClassTable
    populatePrimitives typeClassTable typeClassDAG
    knownRecords <- liftIO $ H.new
    functions <- newFunctionTable
    generics <- emptyGenericState
    return $
        TranslationState
            { currentSpan = Nothing
            , currentFile
            , translationLogLevel
            , stringUniqifier
            , translationReports = []
            , typeClassDAG
            , typeClassTable
            , holes = mempty
            , variableStates = mempty
            , variableNameMap
            , genericNameMap
            , knownRecords
            , functions
            , generics
            , insideRegion = False
            }

{- |
Execute a computation with a set of generic variables in scope.
This temporarily updates the 'genericNameMap' in the translation state.
If a generic name shadows an existing one, the old one is restored after the computation.
-}
withGenericContext :: [(Identifier, GenericID)] -> Tyck a -> Tyck a
withGenericContext newGenerics action = do
    oldMap <- State.gets genericNameMap
    let newMap = foldl' (\m (k, v) -> HashMap.insert k v m) oldMap newGenerics
    State.modify $ \st -> st{genericNameMap = newMap}

    result <- action

    State.modify $ \st -> st{genericNameMap = oldMap}
    return result

translateGeneric :: (Identifier, [Path]) -> Tyck (Identifier, GenericID)
translateGeneric (identifier, bounds) = do
    st <- State.gets generics
    gid <- newGenericVar identifier Nothing bounds st -- TODO: handle span
    return (identifier, gid)

scanStmt :: Syn.Stmt -> Tyck ()
scanStmt (Syn.SpannedStmt s) = do
    backup <- State.gets currentSpan
    State.modify $ \st -> st{currentSpan = Just (spanStartOffset s, spanEndOffset s)}
    scanStmt (spanValue s)
    State.modify $ \st -> st{currentSpan = backup}
scanStmt (Syn.RecordStmt record) = do
    let name = Syn.recordName record
    let tyParams = Syn.recordTyParams record

    -- Translate generics
    genericsList <- mapM translateGeneric tyParams

    -- Translate fields/variants within generic context
    withGenericContext genericsList $ do
        fields <- case Syn.recordFields record of
            Syn.Named fs -> do
                fs' <- mapM (\(n, t, f) -> (n,,f) <$> evalType t) fs
                return $ Sem.Named fs'
            Syn.Unnamed fs -> do
                fs' <- mapM (\(t, f) -> (,f) <$> evalType t) fs
                return $ Sem.Unnamed fs'
            Syn.Variants vs -> do
                vs' <- mapM (\(n, ts) -> (n,) <$> mapM evalType ts) vs
                return $ Sem.Variants vs'

        let kind = case Syn.recordKind record of
                Syn.StructKind -> Sem.StructKind
                Syn.EnumKind -> Sem.EnumKind

        let semRecord =
                Sem.Record
                    { Sem.recordName = Path name [] -- TODO: handle module path
                    , Sem.recordTyParams = genericsList
                    , Sem.recordFields = fields
                    , Sem.recordKind = kind
                    , Sem.recordVisibility = Syn.recordVisibility record
                    , Sem.recordDefaultCap = Syn.recordDefaultCap record
                    }

        let path = Path name [] -- TODO: handle module path
        addRecordDefinition path semRecord
scanStmt
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
                    Nothing -> Sem.TypeUnit
            pendingFuncBody <- newIORef' Nothing
            funcSpan <- State.gets currentSpan
            let proto =
                    Sem.FunctionProto
                        { Sem.funcVisibility = funcVisibility
                        , Sem.funcName = funcName
                        , Sem.funcGenerics = genericsList
                        , Sem.funcParams = paramsList
                        , Sem.funcReturnType = returnTy
                        , Sem.funcIsRegional = funcIsRegional
                        , Sem.funcBody = pendingFuncBody
                        , Sem.funcSpan = funcSpan
                        }
            -- TODO: handle module path
            let funcPath = Path funcName []
            functionTable <- State.gets functions
            existed <-
                isJust <$> liftIO (H.lookup (Sem.functionProtos functionTable) funcPath)
            if existed
                then
                    reportError $
                        "Function already defined: " <> unIdentifier funcName
                else liftIO $ H.insert (Sem.functionProtos functionTable) funcPath proto
      where
        translateParam ::
            (Identifier, Syn.Type, Syn.FlexFlag) -> Tyck (Identifier, Sem.Type)
        translateParam (paramName, ty, flex) = do
            ty' <- evalTypeWithFlexivity ty flex
            return (paramName, ty')

scanProg :: Syn.Prog -> Tyck ()
scanProg = flip forM_ scanStmt

exprWithSpan :: Sem.Type -> Sem.ExprKind -> Tyck Sem.Expr
exprWithSpan exprType exprKind = do
    exprSpan <- currentSpan <$> State.get
    return $ Sem.Expr{exprKind, exprSpan, exprType}

allocateNewString :: T.Text -> Tyck StringToken
allocateNewString str = do
    uniqifier <- stringUniqifier <$> State.get
    strToToken str uniqifier

addReport :: Report -> Tyck ()
addReport report = do
    State.modify $ \st ->
        st{translationReports = report : translationReports st}

reportError :: T.Text -> Tyck ()
reportError msg = do
    st <- State.get
    let span' = currentSpan st
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
    addReport report

-- convert syntax type to semantic type
evalType :: Syn.Type -> Tyck Sem.Type
evalType (Syn.TypeSpanned s) = evalType (spanValue s)
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
                reportError $
                    "Generic type "
                        <> unIdentifier (pathBasename path)
                        <> " cannot have type arguments"
            return $ Sem.TypeGeneric gid
        Nothing -> do
            knownRecords <- State.gets knownRecords
            liftIO (H.lookup knownRecords path) >>= \case
                Just record -> case Sem.recordDefaultCap record of
                    Value -> return $ Sem.TypeRecord path args'
                    Shared -> return $ Sem.TypeRc (Sem.TypeRecord path args') Shared
                    Regional -> do
                        return $ Sem.TypeRc (Sem.TypeRecord path args') Regional
                    cap -> do
                        reportError $ "Unsupported capability: " <> T.pack (show cap)
                        return Sem.TypeBottom
                Nothing -> do
                    reportError $ "Unknown type: " <> T.pack (show path)
                    return Sem.TypeBottom
evalType (Syn.TypeIntegral (Syn.Signed n)) = return $ Sem.TypeIntegral (Sem.Signed n)
evalType (Syn.TypeIntegral (Syn.Unsigned n)) = return $ Sem.TypeIntegral (Sem.Unsigned n)
evalType (Syn.TypeFP (Syn.IEEEFloat n)) = return $ Sem.TypeFP (Sem.IEEEFloat n)
evalType (Syn.TypeFP Syn.BFloat16) = return $ Sem.TypeFP Sem.BFloat16
evalType (Syn.TypeFP Syn.Float8) = return $ Sem.TypeFP Sem.Float8
evalType Syn.TypeBool = return Sem.TypeBool
evalType Syn.TypeStr = return Sem.TypeStr
evalType Syn.TypeUnit = return Sem.TypeUnit
evalType Syn.TypeBottom = return Sem.TypeBottom
evalType (Syn.TypeArrow t1 t2) = do
    t1' <- evalType t1
    (args, ret) <- unfoldArrow t2
    return $ Sem.TypeClosure (t1' : args) ret
  where
    unfoldArrow :: Syn.Type -> Tyck ([Sem.Type], Sem.Type)
    unfoldArrow (Syn.TypeArrow a b) = do
        a' <- evalType a
        (args, ret) <- unfoldArrow b
        return (a' : args, ret)
    unfoldArrow (Syn.TypeSpanned s) = unfoldArrow (spanValue s)
    unfoldArrow t = do
        t' <- evalType t
        return ([], t')

evalTypeWithFlexivity :: Syn.Type -> Bool -> Tyck Sem.Type
evalTypeWithFlexivity t isFlexible = do
    t' <- evalType t
    case t' of
        Sem.TypeRc ty Regional -> do
            if isFlexible
                then return $ Sem.TypeRc ty Flex
                else return $ Sem.TypeRc ty Rigid
        _ -> return t'

forceAndCheckHoles :: Sem.Type -> Tyck Sem.Type
forceAndCheckHoles ty = do
    ty' <- force ty
    case ty' of
        Sem.TypeHole _ -> do
            reportError "Unsolved type hole"
            return ty'
        Sem.TypeRecord path args -> do
            args' <- mapM forceAndCheckHoles args
            return $ Sem.TypeRecord path args'
        Sem.TypeClosure args ret -> do
            args' <- mapM forceAndCheckHoles args
            ret' <- forceAndCheckHoles ret
            return $ Sem.TypeClosure args' ret'
        Sem.TypeRc t cap -> do
            t' <- forceAndCheckHoles t
            return $ Sem.TypeRc t' cap
        Sem.TypeRef t cap -> do
            t' <- forceAndCheckHoles t
            return $ Sem.TypeRef t' cap
        _ -> return ty'

wellTypedExpr :: Sem.Expr -> Tyck Sem.Expr
wellTypedExpr expr = do
    ty' <- forceAndCheckHoles (Sem.exprType expr)
    kind' <- case Sem.exprKind expr of
        Sem.GlobalStr s -> return $ Sem.GlobalStr s
        Sem.Constant c -> return $ Sem.Constant c
        Sem.Negate e -> Sem.Negate <$> wellTypedExpr e
        Sem.Not e -> Sem.Not <$> wellTypedExpr e
        Sem.Arith e1 op e2 -> Sem.Arith <$> wellTypedExpr e1 <*> pure op <*> wellTypedExpr e2
        Sem.Cmp e1 op e2 -> Sem.Cmp <$> wellTypedExpr e1 <*> pure op <*> wellTypedExpr e2
        Sem.Cast e t -> do
            e' <- wellTypedExpr e
            t' <- forceAndCheckHoles t
            return $ Sem.Cast e' t'
        Sem.ScfIfExpr e1 e2 e3 -> Sem.ScfIfExpr <$> wellTypedExpr e1 <*> wellTypedExpr e2 <*> wellTypedExpr e3
        Sem.Var v -> return $ Sem.Var v
        Sem.RcWrap e cap -> Sem.RcWrap <$> wellTypedExpr e <*> pure cap
        Sem.ProjChain e idxs -> Sem.ProjChain <$> wellTypedExpr e <*> pure idxs
        Sem.Let span' varID name val body -> do
            val' <- wellTypedExpr val
            body' <- wellTypedExpr body
            return $ Sem.Let span' varID name val' body'
        Sem.FuncCall target tyArgs args regional -> do
            tyArgs' <- mapM forceAndCheckHoles tyArgs
            args' <- mapM wellTypedExpr args
            return $ Sem.FuncCall target tyArgs' args' regional
        Sem.CtorCall path tyArgs variant args -> do
            tyArgs' <- mapM forceAndCheckHoles tyArgs
            args' <- mapM wellTypedExpr args
            return $ Sem.CtorCall path tyArgs' variant args'
        Sem.Poison -> return Sem.Poison
        Sem.RunRegion e -> Sem.RunRegion <$> wellTypedExpr e
    return $ expr{Sem.exprType = ty', Sem.exprKind = kind'}

addRecordDefinition :: Path -> Sem.Record -> Tyck ()
addRecordDefinition path record = do
    records <- State.gets knownRecords
    liftIO $ H.insert records path record

substituteTypeParams :: Sem.Type -> IntMap.IntMap Sem.Type -> Sem.Type
substituteTypeParams ty subst = go ty
  where
    go (Sem.TypeGeneric (GenericID gid)) =
        case IntMap.lookup (fromIntegral gid) subst of
            Just t -> t
            Nothing -> Sem.TypeGeneric (GenericID gid)
    go (Sem.TypeRecord path args) = Sem.TypeRecord path (map go args)
    go (Sem.TypeClosure args ret) = Sem.TypeClosure (map go args) (go ret)
    go (Sem.TypeRc t cap) = Sem.TypeRc (go t) cap
    go (Sem.TypeRef t cap) = Sem.TypeRef (go t) cap
    go t = t

getGenericBound :: GenericID -> Tyck TypeBound
getGenericBound (GenericID gid) = do
    genericsState <- State.gets generics
    varState <- readIORef' (getStateRef genericsState)
    case Seq.lookup (fromIntegral gid) varState of
        Just var -> return $ map Class $ genericBounds var
        Nothing -> do
            reportError $ "Generic ID not found: " <> T.pack (show gid)
            return []

-- Recursively analyze generic flow in an expression
-- We focus on function call and ctor call: at each call site, we examine:
-- 1. If a type fulfills isConcrete, we use addConcreteFlow to add this type as
--    a concrete instance to the generic variable.
-- 2. If otherwise, we add a flow edge from the types generics (collectGenerics) to the callee generic
--    i. if the type is directly a generic, we use addDirectLink
--    ii. otherwise we use addCtorLink
analyzeGenericFlowInExpr :: Sem.Expr -> Tyck ()
analyzeGenericFlowInExpr expr = do
    case Sem.exprKind expr of
        Sem.FuncCall target tyArgs args _ -> do
            -- Analyze arguments
            mapM_ analyzeGenericFlowInExpr args

            -- Analyze generic flow
            functionTable <- State.gets functions
            liftIO (H.lookup (Sem.functionProtos functionTable) target) >>= \case
                Just proto -> do
                    let generics = Sem.funcGenerics proto
                    analyzeGenericInstantiationFlow generics tyArgs
                Nothing -> pure () -- Should have been caught by type checker
        Sem.CtorCall target tyArgs _ args -> do
            -- Analyze arguments
            mapM_ analyzeGenericFlowInExpr args

            -- Analyze generic flow
            knownRecords <- State.gets knownRecords
            liftIO (H.lookup knownRecords target) >>= \case
                Just record -> do
                    let generics = Sem.recordTyParams record
                    analyzeGenericInstantiationFlow generics tyArgs
                Nothing -> pure () -- Should have been caught by type checker

        -- Recursive cases
        Sem.Negate e -> analyzeGenericFlowInExpr e
        Sem.Not e -> analyzeGenericFlowInExpr e
        Sem.Arith e1 _ e2 -> analyzeGenericFlowInExpr e1 >> analyzeGenericFlowInExpr e2
        Sem.Cmp e1 _ e2 -> analyzeGenericFlowInExpr e1 >> analyzeGenericFlowInExpr e2
        Sem.Cast e _ -> analyzeGenericFlowInExpr e
        Sem.ScfIfExpr e1 e2 e3 -> do
            analyzeGenericFlowInExpr e1
            analyzeGenericFlowInExpr e2
            analyzeGenericFlowInExpr e3
        Sem.RcWrap e _ -> analyzeGenericFlowInExpr e
        Sem.ProjChain e _ -> analyzeGenericFlowInExpr e
        Sem.Let _ _ _ val body -> do
            analyzeGenericFlowInExpr val
            analyzeGenericFlowInExpr body
        Sem.RunRegion e -> analyzeGenericFlowInExpr e
        -- Base cases
        Sem.GlobalStr _ -> pure ()
        Sem.Constant _ -> pure ()
        Sem.Var _ -> pure ()
        Sem.Poison -> pure ()
analyzeGenericInstantiationFlow ::
    [(Identifier, GenericID)] -> [Sem.Type] -> Tyck ()
analyzeGenericInstantiationFlow genericParams tyArgs = do
    genericState <- State.gets generics
    zipWithM_
        ( \(_, gid) ty -> do
            if Sem.isConcrete ty
                then addConcreteFlow gid ty genericState
                else do
                    let srcGenerics = Sem.collectGenerics IntSet.empty ty
                    let srcGenericIDs = map (GenericID . fromIntegral) $ IntSet.toList srcGenerics
                    forM_ srcGenericIDs $ \srcID -> do
                        case ty of
                            Sem.TypeGeneric g | g == srcID -> addDirectLink srcID gid genericState
                            _ -> addCtorLink srcID gid ty genericState
        )
        genericParams
        tyArgs

analyzeGenericFlowInType :: Sem.Type -> Tyck ()
analyzeGenericFlowInType (Sem.TypeRecord path args) = do
    knownRecords <- State.gets knownRecords
    mRecord <- liftIO $ H.lookup knownRecords path
    case mRecord of
        Just record -> do
            let generics = Sem.recordTyParams record
            analyzeGenericInstantiationFlow generics args
        Nothing -> pure ()
    mapM_ analyzeGenericFlowInType args
analyzeGenericFlowInType (Sem.TypeClosure args ret) = do
    mapM_ analyzeGenericFlowInType args
    analyzeGenericFlowInType ret
analyzeGenericFlowInType (Sem.TypeRc t _) = analyzeGenericFlowInType t
analyzeGenericFlowInType (Sem.TypeRef t _) = analyzeGenericFlowInType t
analyzeGenericFlowInType _ = pure ()

analyzeGenericFlowInRecord :: Sem.Record -> Tyck ()
analyzeGenericFlowInRecord record = do
    let types = case Sem.recordFields record of
            Sem.Named fs -> map (\(_, t, _) -> t) fs
            Sem.Unnamed fs -> map (\(t, _) -> t) fs
            Sem.Variants vs -> concatMap snd vs
    mapM_ analyzeGenericFlowInType types

-- Analyze generic flow for the whole translation module.
analyzeGenericFlow :: Tyck ()
analyzeGenericFlow = do
    functionTable <- State.gets functions
    protos <- liftIO $ H.toList (Sem.functionProtos functionTable)
    forM_ protos $ \(_, proto) -> do
        mBody <- readIORef' (Sem.funcBody proto)
        case mBody of
            Just body -> analyzeGenericFlowInExpr body
            Nothing -> pure ()

    knownRecords <- State.gets knownRecords
    records <- liftIO $ H.toList knownRecords
    forM_ records $ \(_, record) -> analyzeGenericFlowInRecord record

solveAllGenerics :: Tyck (Maybe GenericSolution)
solveAllGenerics = do
    L.logTrace_ "Solving all generics"
    analyzeGenericFlow
    genericState <- State.gets generics
    solveGeneric genericState >>= \case
        Right (x, y, ty) -> do
            -- TODO: better format report
            reportError $
                "Growing edge detected between generic variables: "
                    <> T.pack (show x)
                    <> " -> "
                    <> T.pack (show y)
                    <> " via type "
                    <> T.pack (show ty)
            return Nothing
        Left table -> do
            L.logInfo_ "Generic solving succeeded"
            return (Just table)

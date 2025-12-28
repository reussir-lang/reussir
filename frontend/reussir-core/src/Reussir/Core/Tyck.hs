{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core.Tyck where

import Data.Digest.XXHash.FFI (XXH3 (XXH3))
import Data.Function ((&))
import Data.HashTable.IO qualified as H
import Data.Hashable (Hashable (hash))
import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen.Intrinsics qualified as Intrinsic
import Reussir.Codegen.Intrinsics.Arith qualified as Arith
import Reussir.Core.Types.Expr qualified as Sem
import Reussir.Core.Types.String (StringToken, StringUniqifier (..))
import Reussir.Core.Types.Type qualified as Sem
import Reussir.Diagnostic (Label (Error), Report (..))
import Reussir.Diagnostic.Report (
    addForegroundColorToCodeRef,
    addForegroundColorToText,
    annotatedCodeRef,
    defaultCodeRef,
    defaultText,
 )
import Reussir.Parser.Types.Expr qualified as Syn
import Reussir.Parser.Types.Lexer (WithSpan (..))
import System.Console.ANSI.Types qualified as ANSI

strToToken :: (IOE :> es) => T.Text -> StringUniqifier -> Eff es StringToken
strToToken str (StringUniqifier table) = do
    let h = hash (XXH3 str)
    bucket <- liftIO $ H.lookup table h
    case bucket of
        Just seqs -> case Seq.elemIndexL str seqs of
            Just idx -> return (h, fromIntegral idx)
            Nothing -> do
                let newSeqs = seqs Seq.|> str
                liftIO $ H.insert table h newSeqs
                return (h, fromIntegral (Seq.length newSeqs - 1))
        Nothing -> do
            liftIO $ H.insert table h (Seq.singleton str)
            return (h, 0)

data TranslationState = TranslationState
    { currentSpan :: Maybe (Int64, Int64)
    , currentFile :: FilePath
    , stringUniqifier :: StringUniqifier
    , translationReports :: [Report]
    }
    deriving (Show)

emptyTranslationState :: (IOE :> es) => FilePath -> Eff es TranslationState
emptyTranslationState currentFile = do
    table <- liftIO $ H.new
    let stringUniqifier = StringUniqifier table
    return $
        TranslationState
            { currentSpan = Nothing
            , currentFile
            , stringUniqifier
            , translationReports = []
            }

type Tyck = Eff '[IOE, State.State TranslationState] -- TODO: Define effects used in type checking

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

inferType :: Syn.Expr -> Tyck Sem.Expr
--
--  ───────────────────
--       int <- 123
inferType (Syn.ConstExpr (Syn.ConstInt value)) = do
    let ty = Sem.TypeIntegral $ Sem.Signed 64 -- TODO: we force i64 for now
    exprWithSpan ty $ Sem.Constant (fromIntegral value)
--
--  ─────────────────────
--       f64 <- 1.23
inferType (Syn.ConstExpr (Syn.ConstDouble value)) = do
    let ty = Sem.TypeFP $ Sem.IEEEFloat 64 -- TODO: we force f64 for now
    exprWithSpan ty $ Sem.Constant (realToFrac value)
--
--  ─────────────────────
--     str <- "string"
inferType (Syn.ConstExpr (Syn.ConstString value)) = do
    token <- allocateNewString value
    let ty = Sem.TypeStr
    exprWithSpan ty $ Sem.GlobalStr token
--
--  ─────────────────────
--    bool <- true/false
inferType (Syn.ConstExpr (Syn.ConstBool value)) = do
    let ty = Sem.TypeBool
    exprWithSpan ty $ Sem.Constant (if value then 1 else 0)
--     x -> bool
--  ─────────────────────
--    bool <- !x
inferType (Syn.UnaryOpExpr Syn.Not subExpr) = do
    let ty = Sem.TypeBool
    subExpr' <- checkType subExpr ty
    one <- exprWithSpan ty $ Sem.Constant 1
    let call =
            Sem.IntrinsicCall (Intrinsic.Arith Arith.Xori) [subExpr', one]
    exprWithSpan ty call
--     x -> int
--  ─────────────────────
--    int <- (-x)
inferType (Syn.UnaryOpExpr Syn.Negate subExpr) = do
    let ty = Sem.TypeIntegral $ Sem.Signed 64 -- TODO: we force i64 for now
    subExpr' <- checkType subExpr ty
    zero <- exprWithSpan ty $ Sem.Constant 0
    let callTarget = Intrinsic.Arith $ Arith.Subi Arith.iofNone
    let call = Sem.IntrinsicCall callTarget [zero, subExpr']
    exprWithSpan ty call
inferType (Syn.BinOpExpr op lhs rhs) = inferTypeBinOp op lhs rhs
inferType (Syn.SpannedExpr (WithSpan subExpr start end)) = do
    oldSpan <- currentSpan <$> State.get
    State.modify $ \st -> st{currentSpan = Just (start, end)}
    res <- inferType subExpr
    State.modify $ \st -> st{currentSpan = oldSpan}
    return res
inferType e = error $ "unimplemented inference for:\n\t" ++ show e

-- Binary operations
--          T <- x, y -> T, T = int/float
--  ────────────────────────────────────────────────── -- TODO: better bound
--    T <- (x + y) / (x - y) / (x * y) / (x / y) ...
-- Comparison operations
--          T <- x, y -> T, T = int/float
--  ────────────────────────────────────────────────── -- TODO: better bound
--    bool <- (x == y) / (x != y) / (x < y) ...
-- Short-circuit operations
--          x-> bool, y -> bool
--  ────────────────────────────────────────────────── -- TODO: better bound
--    bool <- (x || y) / (x && y)
inferTypeBinOp :: Syn.BinaryOp -> Syn.Expr -> Syn.Expr -> Tyck Sem.Expr
inferTypeBinOp Syn.And lhs rhs = do
    lhs' <- checkType lhs Sem.TypeBool
    rhs' <- checkType rhs Sem.TypeBool
    false <- exprWithSpan Sem.TypeBool $ Sem.Constant 0
    exprWithSpan Sem.TypeBool $ Sem.ScfIfExpr lhs' rhs' false
inferTypeBinOp Syn.Or lhs rhs = do
    lhs' <- checkType lhs Sem.TypeBool
    rhs' <- checkType rhs Sem.TypeBool
    true <- exprWithSpan Sem.TypeBool $ Sem.Constant 1
    exprWithSpan Sem.TypeBool $ Sem.ScfIfExpr lhs' true rhs'
inferTypeBinOp op lhs rhs = do
    lhs' <- inferType lhs
    let lhsTy = Sem.exprType lhs'
    rhs' <- checkType rhs lhsTy
    case lhsTy of
        Sem.TypeIntegral (Sem.Signed _) -> handleIntOp op lhs' rhs' lhsTy True
        Sem.TypeIntegral (Sem.Unsigned _) -> handleIntOp op lhs' rhs' lhsTy False
        Sem.TypeFP _ -> handleFPOp op lhs' rhs' lhsTy
        _ -> do
            reportError $ "Unsupported type for binary operation: " <> T.pack (show lhsTy)
            exprWithSpan Sem.TypeBottom Sem.Poison
  where
    handleIntOp op' lhs' rhs' ty isSigned = do
        let (intrinsic, resTy) = case op' of
                Syn.Add -> (Just $ Intrinsic.Arith $ Arith.Addi Arith.iofNone, ty)
                Syn.Sub -> (Just $ Intrinsic.Arith $ Arith.Subi Arith.iofNone, ty)
                Syn.Mul -> (Just $ Intrinsic.Arith $ Arith.Muli Arith.iofNone, ty)
                Syn.Div ->
                    ( Just $
                        Intrinsic.Arith $
                            if isSigned then Arith.Divsi else Arith.Divui
                    , ty
                    )
                Syn.Mod ->
                    ( Just $
                        Intrinsic.Arith $
                            if isSigned then Arith.Remsi else Arith.Remui
                    , ty
                    )
                Syn.Equ -> (Just $ Intrinsic.Arith $ Arith.Cmpi Arith.CIEq, Sem.TypeBool)
                Syn.Neq -> (Just $ Intrinsic.Arith $ Arith.Cmpi Arith.CINe, Sem.TypeBool)
                Syn.Lt ->
                    ( Just $
                        Intrinsic.Arith $
                            Arith.Cmpi (if isSigned then Arith.CISlt else Arith.CIUlt)
                    , Sem.TypeBool
                    )
                Syn.Gt ->
                    ( Just $
                        Intrinsic.Arith $
                            Arith.Cmpi (if isSigned then Arith.CISgt else Arith.CIUgt)
                    , Sem.TypeBool
                    )
                Syn.Lte ->
                    ( Just $
                        Intrinsic.Arith $
                            Arith.Cmpi (if isSigned then Arith.CISle else Arith.CIUle)
                    , Sem.TypeBool
                    )
                Syn.Gte ->
                    ( Just $
                        Intrinsic.Arith $
                            Arith.Cmpi (if isSigned then Arith.CISge else Arith.CIUge)
                    , Sem.TypeBool
                    )
                _ -> (Nothing, Sem.TypeBottom)
        case intrinsic of
            Just intr -> exprWithSpan resTy $ Sem.IntrinsicCall intr [lhs', rhs']
            Nothing -> do
                reportError "Unsupported integer binary operation"
                exprWithSpan Sem.TypeBottom Sem.Poison

    handleFPOp op' lhs' rhs' ty = do
        let (intrinsic, resTy) = case op' of
                Syn.Add -> (Just $ Intrinsic.Arith $ Arith.Addf (Arith.FastMathFlag 0), ty)
                Syn.Sub -> (Just $ Intrinsic.Arith $ Arith.Subf (Arith.FastMathFlag 0), ty)
                Syn.Mul -> (Just $ Intrinsic.Arith $ Arith.Mulf (Arith.FastMathFlag 0), ty)
                Syn.Div -> (Just $ Intrinsic.Arith $ Arith.Divf (Arith.FastMathFlag 0), ty)
                Syn.Mod -> (Just $ Intrinsic.Arith $ Arith.Remf (Arith.FastMathFlag 0), ty)
                Syn.Equ -> (Just $ Intrinsic.Arith $ Arith.Cmpf Arith.CFOeq (Arith.FastMathFlag 0), Sem.TypeBool)
                Syn.Neq -> (Just $ Intrinsic.Arith $ Arith.Cmpf Arith.CFOne (Arith.FastMathFlag 0), Sem.TypeBool)
                Syn.Lt -> (Just $ Intrinsic.Arith $ Arith.Cmpf Arith.CFOlt (Arith.FastMathFlag 0), Sem.TypeBool)
                Syn.Gt -> (Just $ Intrinsic.Arith $ Arith.Cmpf Arith.CFOgt (Arith.FastMathFlag 0), Sem.TypeBool)
                Syn.Lte -> (Just $ Intrinsic.Arith $ Arith.Cmpf Arith.CFOle (Arith.FastMathFlag 0), Sem.TypeBool)
                Syn.Gte -> (Just $ Intrinsic.Arith $ Arith.Cmpf Arith.CFOge (Arith.FastMathFlag 0), Sem.TypeBool)
                _ -> (Nothing, Sem.TypeBottom)
        case intrinsic of
            Just intr -> exprWithSpan resTy $ Sem.IntrinsicCall intr [lhs', rhs']
            Nothing -> do
                reportError "Unsupported floating point binary operation"
                exprWithSpan Sem.TypeBottom Sem.Poison

checkType :: Syn.Expr -> Sem.Type -> Tyck Sem.Expr
checkType (Syn.SpannedExpr (WithSpan subExpr start end)) ty = do
    oldSpan <- currentSpan <$> State.get
    State.modify $ \st -> st{currentSpan = Just (start, end)}
    res <- checkType subExpr ty
    State.modify $ \st -> st{currentSpan = oldSpan}
    return res
checkType expr ty = do
    -- this is apparently not complete. We need to handle lambda/unification
    expr' <- inferType expr
    let exprTy = Sem.exprType expr'
    if exprTy /= ty
        then do
            reportError $
                "Type mismatch: expected "
                    <> T.pack (show ty)
                    <> ", got "
                    <> T.pack (show exprTy)
            exprWithSpan Sem.TypeBottom Sem.Poison
        else return expr'

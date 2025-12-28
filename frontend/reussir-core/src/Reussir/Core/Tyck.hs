module Reussir.Core.Tyck where

import Data.Digest.XXHash.FFI (XXH3 (XXH3))
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
import Reussir.Diagnostic.Report (Report)
import Reussir.Parser.Types.Expr qualified as Syn

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
    , stringUniqifier :: StringUniqifier
    , translationReports :: [Report]
    }
    deriving (Show)

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

inferType :: Syn.Expr -> Tyck Sem.Expr
inferType (Syn.ConstExpr (Syn.ConstInt value)) = do
    let ty = Sem.TypeIntegral $ Sem.Signed 64 -- TODO: we force i64 for now
    exprWithSpan ty $ Sem.Constant (fromIntegral value)
inferType (Syn.ConstExpr (Syn.ConstDouble value)) = do
    let ty = Sem.TypeFP $ Sem.IEEEFloat 64 -- TODO: we force f64 for now
    exprWithSpan ty $ Sem.Constant (realToFrac value)
inferType (Syn.ConstExpr (Syn.ConstString value)) = do
    token <- allocateNewString value
    let ty = Sem.TypeStr
    exprWithSpan ty $ Sem.GlobalStr token
inferType (Syn.ConstExpr (Syn.ConstBool value)) = do
    let ty = Sem.TypeBool
    exprWithSpan ty $ Sem.Constant (if value then 1 else 0)
inferType (Syn.UnaryOpExpr Syn.Not subExpr) = do
    let ty = Sem.TypeBool
    subExpr' <- checkType subExpr ty
    one <- exprWithSpan ty $ Sem.Constant 1
    let call =
            Sem.IntrinsicCall (Intrinsic.Arith Arith.Xori) [subExpr', one]
    exprWithSpan ty call
inferType (Syn.UnaryOpExpr Syn.Negate subExpr) = do
    let ty = Sem.TypeIntegral $ Sem.Signed 64 -- TODO: we force i64 for now
    subExpr' <- checkType subExpr ty
    zero <- exprWithSpan ty $ Sem.Constant 0
    let callTarget = Intrinsic.Arith $ Arith.Subi Arith.iofNone
    let call = Sem.IntrinsicCall callTarget [zero, subExpr']
    exprWithSpan ty call
inferType _ = undefined

checkType :: Syn.Expr -> Sem.Type -> Tyck Sem.Expr
checkType expr ty = undefined

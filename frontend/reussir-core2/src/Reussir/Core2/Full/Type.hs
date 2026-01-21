module Reussir.Core2.Full.Type where

import Data.Either (partitionEithers)
import Data.HashTable.IO qualified as H
import Data.Int (Int64)
import Data.IntMap.Strict qualified as IntMap
import Effectful (Eff, IOE, liftIO, (:>))
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Codegen.Type (Capability (..))
import Reussir.Core2.Data.Full.Error (Error (..), ErrorKind (..))
import Reussir.Core2.Data.Full.Record (SemiRecordTable)
import Reussir.Core2.Data.Full.Type (GenericMap, Type (..))
import Reussir.Core2.Data.Semi.Record qualified as Semi
import Reussir.Core2.Data.Semi.Type qualified as Semi
import Reussir.Core2.Data.UniqueID (GenericID (..))
import Reussir.Core2.Semi.Mangle (mangleABIName)
import Reussir.Core2.Semi.Type qualified as Semi
import Reussir.Parser.Types.Capability qualified as Syn
import Prelude hiding (span)

convertCapability :: Syn.Capability -> Capability
convertCapability Syn.Value = Value
convertCapability Syn.Shared = Shared
convertCapability Syn.Regional = Regional
convertCapability Syn.Flex = Flex
convertCapability Syn.Rigid = Rigid
convertCapability Syn.Field = Field
convertCapability Syn.Unspecified = Unspecified

convertSemiType :: (IOE :> es) => (Int64, Int64) -> GenericMap -> SemiRecordTable -> Semi.Type -> Eff es (Either [Error] Type)
convertSemiType span genericMap semiRecords semiType = do
    let wrapError kind = [Error span kind]
    case semiType of
        Semi.TypeHole _ -> error "Type hole is not allowed in conversion"
        Semi.TypeRecord path typeArgs flex -> do
            let instantiatedTyArgs = map (flip Semi.substituteGenericMap genericMap) typeArgs
                mangledSymbol = mangleABIName (Semi.TypeRecord path instantiatedTyArgs flex)
                symbol = verifiedSymbol mangledSymbol
            record <- liftIO $ H.lookup semiRecords path
            record' <- case record of
                Nothing -> error $ "Record " ++ show path ++ " not found in semi records"
                Just r -> pure r
            let defaultCap = Semi.recordDefaultCap record'
            case (defaultCap, flex) of
                (Syn.Value, _) -> pure $ Right $ TypeRecord symbol
                (Syn.Shared, Semi.Irrelevant) -> pure $ Right $ TypeRc (TypeRecord symbol) Shared
                (Syn.Regional, Semi.Regional) -> pure $ Right $ TypeRc (TypeRecord symbol) Regional
                (Syn.Regional, Semi.Flex) -> pure $ Right $ TypeRc (TypeRecord symbol) Flex
                (Syn.Regional, Semi.Rigid) -> pure $ Right $ TypeRc (TypeRecord symbol) Rigid
                _ -> pure $ Left $ wrapError (InvalidCapability path defaultCap)
        Semi.TypeIntegral int -> pure $ Right $ TypeIntegral int
        Semi.TypeFP fp -> pure $ Right $ TypeFP fp
        Semi.TypeBool -> pure $ Right TypeBool
        Semi.TypeStr -> pure $ Right TypeStr
        Semi.TypeUnit -> pure $ Right TypeUnit
        Semi.TypeClosure args ret -> do
            args' <- mapM (convertSemiType span genericMap semiRecords) args
            ret' <- convertSemiType span genericMap semiRecords ret
            let (argErrs, argSuccs) = partitionEithers args'
            case (argErrs, ret') of
                ([], Right r) -> pure $ Right $ TypeRc (TypeClosure argSuccs r) Shared
                (errs, Left rErr) -> pure $ Left $ (concat errs) ++ rErr
                (errs, _) -> pure $ Left $ concat errs
        Semi.TypeGeneric (GenericID gid) -> case (IntMap.lookup (fromIntegral gid) genericMap) of
            Nothing -> pure $ Left $ wrapError (UnknownGeneric (GenericID gid))
            Just ty -> convertSemiType span genericMap semiRecords ty
        Semi.TypeBottom -> pure $ Right TypeBottom
        Semi.TypeNullable x -> do
            x' <- convertSemiType span genericMap semiRecords x
            case x' of
                Left err -> pure $ Left err
                Right ty -> case ty of
                    TypeRc{} -> pure $ Right $ TypeNullable ty
                    _ -> pure $ Left $ wrapError (InvalidNullableType semiType)

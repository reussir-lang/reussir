{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Reussir.Core2.Full.Record where

import Control.Monad
import Data.Either (partitionEithers)
import Data.HashTable.IO qualified as H
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Vector.Strict qualified as V
import Effectful (Eff, IOE, liftIO, (:>))
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Codegen.Type (Capability (Regional))
import Reussir.Core2.Data.Full.Record (FieldFlag, FullRecordTable, InvalidRecInstantiation (..), Record (..), RecordFields (..), RecordKind (..), SemiRecordTable)
import Reussir.Core2.Data.Full.Type (Type (..))
import Reussir.Core2.Data.Generic (GenericState (..))
import Reussir.Core2.Data.Semi.Record qualified as Semi
import Reussir.Core2.Data.Semi.Type qualified as Semi
import Reussir.Core2.Data.UniqueID (GenericID (..))
import Reussir.Core2.Full.Type (convertCapability, convertSemiType)
import Reussir.Core2.Semi.Mangle (mangleABIName)
import Reussir.Parser.Types.Lexer (Path (..))

-- Assume that tyArgs is concrete
instantiateRecord :: (IOE :> es) => [Semi.Type] -> SemiRecordTable -> Semi.Record -> Eff es (Either InvalidRecInstantiation Record)
instantiateRecord tyArgs semiRecords (Semi.Record path tyParams fields kind _ cap) = do
    let mangledName = mangleABIName (Semi.TypeRecord path tyArgs Semi.Irrelevant)
    let symbol = verifiedSymbol mangledName
    let genericMap =
            IntMap.fromList $
                zipWith (\tyArg (_, GenericID gid) -> (fromIntegral gid, tyArg)) tyArgs tyParams
    let cap' = convertCapability cap

    instantiatedFields <- instantiateField genericMap fields

    let kind' = case kind of
            Semi.StructKind -> StructKind
            Semi.EnumKind -> EnumKind
            Semi.EnumVariant p i ->
                let parentSymbol = verifiedSymbol $ mangleABIName (Semi.TypeRecord p tyArgs Semi.Irrelevant)
                 in EnumVariant parentSymbol i

    case instantiatedFields of
        Left err -> pure $ Left err
        Right fds ->
            pure $
                Right
                    Record
                        { recordName = symbol
                        , recordRawPath = path
                        , recordSemiTyParams = tyArgs
                        , recordFields = fds
                        , recordKind = kind'
                        , recordDefaultCap = cap'
                        }
  where
    -- Field does not allow rc wrapper as it will uses capability to decide the projected type.
    removeTopLevelRc :: Type -> Type
    removeTopLevelRc (TypeRc ty _) = ty
    removeTopLevelRc ty = ty

    isRecord :: Type -> Bool
    isRecord (TypeRecord _) = True
    isRecord _ = False

    isRegional :: Type -> Bool
    isRegional (TypeRc _ Regional) = True
    isRegional _ = False

    validateField :: (IOE :> es') => Type -> FieldFlag -> Eff es' (Either InvalidRecInstantiation ())
    validateField fullTy isMutable = do
        let dimTy = removeTopLevelRc fullTy
        if isMutable && isRecord dimTy && not (isRegional fullTy)
            then pure $ Left $ InvalidRecInstantiation path tyArgs "invalid capability for regional mutable field"
            else pure $ Right ()

    instantiateField :: (IOE :> es') => IntMap.IntMap Semi.Type -> Semi.RecordFields -> Eff es' (Either InvalidRecInstantiation RecordFields)
    instantiateField genericMap fieldsToInstantiate = case fieldsToInstantiate of
        Semi.Named fs -> do
            results <- for fs \(name, ty, f) -> do
                rawTy <- convertSemiType genericMap semiRecords ty
                validation <- validateField rawTy f
                case validation of
                    Left err -> pure $ Left err
                    Right () -> do
                        let ty' = removeTopLevelRc rawTy
                        pure $ Right (name, ty', f)
            let (errors, successes) = partitionEithers (foldr (\x acc -> case x of Left e -> Left e : acc; Right v -> Right v : acc) [] results)
            case errors of
                (e : _) -> pure $ Left e
                [] -> pure $ Right (Named (V.fromList successes))
        Semi.Unnamed fs -> do
            results <- for fs \(ty, f) -> do
                rawTy <- convertSemiType genericMap semiRecords ty
                validation <- validateField rawTy f
                case validation of
                    Left err -> pure $ Left err
                    Right () -> do
                        let ty' = removeTopLevelRc rawTy
                        pure $ Right (ty', f)
            let (errors, successes) = partitionEithers (foldr (\x acc -> case x of Left e -> Left e : acc; Right v -> Right v : acc) [] results)
            case errors of
                (e : _) -> pure $ Left e
                [] -> pure $ Right (Unnamed (V.fromList successes))
        Semi.Variants vs -> do
            let (Path base segments) = path
            let parentSegments = segments ++ [base]
            vs' <- for vs \v -> do
                let vPath = Path v parentSegments
                let vTy = Semi.TypeRecord vPath tyArgs Semi.Irrelevant
                let vSymbol = verifiedSymbol $ mangleABIName vTy
                pure vSymbol
            pure $ Right $ Variants vs'

convertSemiRecordTable ::
    (IOE :> es) =>
    SemiRecordTable ->
    GenericState ->
    Eff es ([InvalidRecInstantiation], FullRecordTable)
convertSemiRecordTable semiRecords genericState = do
    fullTable <- liftIO H.new
    -- Iterate over all semi records
    recordList <- liftIO $ H.toList semiRecords
    results <- for recordList (processRecord fullTable)
    let errors = concatMap fst results
    pure (errors, fullTable)
  where
    processRecord fullTable (_, record) = do
        let tyParams = Semi.recordTyParams record
        argCombinations <-
            if null tyParams
                then pure [[]]
                else do
                    -- Retrieve solutions for each generic parameter
                    paramSolutions <- for tyParams \(_, GenericID gid) -> do
                        solutions <- liftIO $ H.lookup (concreteFlow genericState) (GenericID gid)
                        pure $ fromMaybe [] solutions

                    -- Compute cross-product
                    pure $ sequence paramSolutions
        when (null argCombinations) $ do
            error "No solutions found for generic parameters"
        -- Instantiate and insert for each combination
        results <- for argCombinations \args -> do
            instantiated <- instantiateRecord args semiRecords record
            case instantiated of
                Right rec -> do
                    liftIO $ H.insert fullTable (recordName rec) rec
                    pure $ Right ()
                Left err -> pure $ Left err

        pure $ partitionEithers results

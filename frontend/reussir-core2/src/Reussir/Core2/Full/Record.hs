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
import Data.Vector.Strict.Mutable qualified as MV
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.State.Static.Local qualified as State
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Codegen.Type (Capability (Regional))
import Reussir.Core2.Data.Full.Context
import Reussir.Core2.Data.Full.Error (Error (..), ErrorKind (..))
import Reussir.Core2.Data.Full.Record (
    FieldFlag,
    Record (..),
    RecordFields (..),
    RecordKind (..),
    SemiRecordTable,
 )
import Reussir.Core2.Data.Full.Type (Type (..))
import Reussir.Core2.Data.Generic (GenericSolution)
import Reussir.Core2.Data.Semi.Record qualified as Semi
import Reussir.Core2.Data.Semi.Type qualified as Semi
import Reussir.Core2.Data.UniqueID (GenericID (..))
import Reussir.Core2.Full.Type (convertCapability, convertSemiType)
import Reussir.Core2.Semi.Mangle (mangleABIName)
import Reussir.Parser.Types.Lexer (Path (..), WithSpan (..))
import Prelude hiding (span)

-- Assume that tyArgs is concrete

-- Assume that tyArgs is concrete
instantiateRecord :: (IOE :> es) => [Semi.Type] -> SemiRecordTable -> Semi.Record -> Eff es (Either [Error] Record)
instantiateRecord tyArgs semiRecords (Semi.Record path tyParams fields kind _ cap recSpan) = do
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
        Left errs -> pure $ Left errs
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
                        , recordSpan = recSpan
                        }
  where
    span = fromMaybe (0, 0) recSpan

    -- Field does not allow rc wrapper as it will uses capability to decide the projected type.
    removeTopLevelRc :: Type -> Type
    removeTopLevelRc (TypeRc ty _) = ty
    removeTopLevelRc ty = ty

    isRegional :: Type -> Bool
    isRegional (TypeRc _ Regional) = True
    isRegional _ = False

    validateField :: (IOE :> es') => Type -> FieldFlag -> Int -> Eff es' (Either Error ())
    validateField fullTy isMutable idx = do
        if isMutable && not (isRegional fullTy)
            then
                pure $
                    Left $
                        Error span $
                            InvalidRecordField
                                { recordPath = path
                                , recordTypeArgs = V.fromList tyArgs
                                , errorIndex = idx
                                }
            else pure $ Right ()

    instantiateField :: (IOE :> es') => IntMap.IntMap Semi.Type -> Semi.RecordFields -> Eff es' (Either [Error] RecordFields)
    instantiateField genericMap fieldsToInstantiate = do
        let
            convertAndValidate acc idx ty f span' mName mv = do
                rawTyResult <- convertSemiType span' genericMap semiRecords ty
                case rawTyResult of
                    Left errs -> pure $ case acc of
                        Left existing -> Left (existing ++ errs)
                        Right () -> Left errs
                    Right rawTy -> do
                        validation <- validateField rawTy f idx
                        case validation of
                            Left err -> pure $ case acc of
                                Left existing -> Left (existing ++ [err])
                                Right () -> Left [err]
                            Right () -> do
                                let ty' = removeTopLevelRc rawTy
                                liftIO $ MV.write mv idx (mName, ty', f)
                                pure acc

            finalizeResult result mv = case result of
                Left errs -> pure $ Left errs
                Right () -> do
                    frozen <- liftIO $ V.freeze mv
                    pure $ Right (Components frozen)

        case fieldsToInstantiate of
            Semi.Named fs -> do
                mv <- liftIO $ MV.new (V.length fs)
                result <-
                    V.ifoldM
                        ( \acc idx (WithSpan (name, ty, f) start end) ->
                            convertAndValidate acc idx ty f (start, end) (Just name) mv
                        )
                        (Right ())
                        fs
                finalizeResult result mv
            Semi.Unnamed fs -> do
                mv <- liftIO $ MV.new (V.length fs)
                result <-
                    V.ifoldM
                        ( \acc idx (WithSpan (ty, f) start end) ->
                            convertAndValidate acc idx ty f (start, end) Nothing mv
                        )
                        (Right ())
                        fs
                finalizeResult result mv
            Semi.Variants vs -> do
                -- Variants usually don't have types to instantiate in this context unless they are enum variants with payloads?
                -- Semi.Variants contains identifiers.
                let (Path base segments) = path
                let parentSegments = segments ++ [base]
                vs' <- for vs \(WithSpan v _ _) -> do
                    let vPath = Path v parentSegments
                    let vTy = Semi.TypeRecord vPath tyArgs Semi.Irrelevant
                    let vSymbol = verifiedSymbol $ mangleABIName vTy
                    pure vSymbol
                pure $ Right $ Variants vs'

convertSemiRecordTable ::
    SemiRecordTable ->
    GenericSolution ->
    GlobalFullEff [Error]
convertSemiRecordTable semiRecords genericSolution = do
    State.modify $ \st -> st{ctxSemiRecords = semiRecords}
    fullTable <- State.gets ctxRecords
    -- Iterate over all semi records
    recordList <- liftIO $ H.toList semiRecords
    results <- for recordList (processRecord fullTable)
    pure $ concatMap fst results
  where
    processRecord fullTable (_, record) = do
        let tyParams = Semi.recordTyParams record
        argCombinations <-
            if null tyParams
                then pure [[]]
                else do
                    -- Retrieve solutions for each generic parameter
                    paramSolutions <- for tyParams \(_, GenericID gid) -> do
                        solutions <- liftIO $ H.lookup genericSolution (GenericID gid)
                        pure $ fromMaybe [] solutions

                    -- Compute cross-product
                    pure $ sequence paramSolutions
        -- Instantiate and insert for each combination
        results <- for argCombinations \args -> do
            instantiated <- instantiateRecord args semiRecords record
            case instantiated of
                Right rec -> do
                    liftIO $ H.insert fullTable (recordName rec) rec
                    pure $ Right ()
                Left errs -> pure $ Left errs

        let (errsList, successes) = partitionEithers results
        pure (concat errsList, successes)

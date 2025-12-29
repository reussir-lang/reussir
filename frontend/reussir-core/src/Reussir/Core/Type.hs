module Reussir.Core.Type where

import Data.HashSet qualified as HashSet
import Data.HashTable.IO qualified as H
import Effectful (Eff, IOE, MonadIO (liftIO), (:>))
import Reussir.Core.Types
import Reussir.Core.Types.Class (Class)

type GenenricOrHole = Either GenericID HoleID

substituteGenericOrHole ::
    Type ->
    (GenenricOrHole -> Maybe Type) ->
    Type
substituteGenericOrHole original subst = go original
  where
    go (TypeRecord path args) = TypeRecord path (map go args)
    go (TypeIntegral it) = TypeIntegral it
    go (TypeFP fpt) = TypeFP fpt
    go TypeBool = TypeBool
    go TypeStr = TypeStr
    go TypeUnit = TypeUnit
    go (TypeClosure args ret) = TypeClosure (map go args) (go ret)
    go (TypeHole hole) = case subst (Right hole) of
        Just t -> t
        Nothing -> TypeHole hole
    go (TypeGeneric generic) = case subst (Left generic) of
        Just t -> t
        Nothing -> TypeGeneric generic
    go (TypeRc t cap) = TypeRc (go t) cap
    go (TypeRef t cap) = TypeRef (go t) cap
    go TypeBottom = TypeBottom

substituteGeneric ::
    Type ->
    (GenericID -> Maybe Type) ->
    Type
substituteGeneric original subst = substituteGenericOrHole original f
  where
    f (Left generic) = subst generic
    f (Right _) = Nothing

substituteHole ::
    Type ->
    (HoleID -> Maybe Type) ->
    Type
substituteHole original subst = substituteGenericOrHole original f
  where
    f (Right hole) = subst hole
    f (Left _) = Nothing

-- check if a type is concrete (generic-free)
isConcrete :: Type -> Bool
isConcrete (TypeRecord _ args) = all isConcrete args
isConcrete (TypeClosure args ret) = all isConcrete args && isConcrete ret
isConcrete (TypeGeneric _) = False
isConcrete (TypeHole _) = False
isConcrete _ = True

-- check if a type is free of holes
isHoleFree :: Type -> Bool
isHoleFree (TypeRecord _ args) = all isHoleFree args
isHoleFree (TypeClosure args ret) = all isHoleFree args && isHoleFree ret
isHoleFree (TypeHole _) = False
isHoleFree _ = True

emptyTypeClassTable :: (IOE :> es) => Eff es TypeClassTable
emptyTypeClassTable = do
    ht <- liftIO H.new
    pure $ TypeClassTable ht

addClassToType ::
    (IOE :> es) =>
    TypeClassTable ->
    Type ->
    Class ->
    Eff es ()
addClassToType (TypeClassTable ht) t cls = liftIO $ do
    mClasses <- H.lookup ht t
    case mClasses of
        Just classes -> H.insert ht t (HashSet.insert cls classes)
        Nothing -> H.insert ht t (HashSet.singleton cls)

getClassesOfType ::
    (IOE :> es) =>
    TypeClassTable ->
    Type ->
    Eff es (HashSet.HashSet Class)
getClassesOfType (TypeClassTable ht) t = do
    mClasses <- liftIO $ H.lookup ht t
    case mClasses of
        Just classes -> pure classes
        Nothing -> pure HashSet.empty

typeHasClass ::
    (IOE :> es) =>
    TypeClassTable ->
    Type ->
    Class ->
    Eff es Bool
typeHasClass table t cls = do
    classes <- getClassesOfType table t
    pure $ HashSet.member cls classes

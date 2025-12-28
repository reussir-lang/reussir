module Reussir.Core.Type where

import Reussir.Core.Types

type GenenricOrHole = Either GenericID HoleID

substituteGenericOrHole ::
    Type ->
    (GenenricOrHole -> Maybe Type) ->
    Type
substituteGenericOrHole original subst = go original
  where
    go (TypeExpr path args) = TypeExpr path (map go args)
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
isConcrete (TypeExpr _ args) = all isConcrete args
isConcrete (TypeClosure args ret) = all isConcrete args && isConcrete ret
isConcrete (TypeGeneric _) = False
isConcrete (TypeHole _) = False
isConcrete _ = True

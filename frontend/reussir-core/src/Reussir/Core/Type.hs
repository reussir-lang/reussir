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
    go (TypeArrow a b) = TypeArrow (go a) (go b)
    go (TypeHole hole) = case subst (Right hole) of
        Just t -> t
        Nothing -> TypeHole hole
    go (TypeGeneric generic) = case subst (Left generic) of
        Just t -> t
        Nothing -> TypeGeneric generic

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
isConcrete (TypeArrow t1 t2) = isConcrete t1 && isConcrete t2
isConcrete (TypeGeneric _) = False
isConcrete _ = True

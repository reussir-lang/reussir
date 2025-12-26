module Reussir.Core.Type where

import Reussir.Core.Types

substituteMeta ::
    Type ->
    (MetaID -> Maybe Type) ->
    Type
substituteMeta original subst = go original
  where
    go (TypeExpr path args) = TypeExpr path (map go args)
    go (TypeIntegral it) = TypeIntegral it
    go (TypeFP fpt) = TypeFP fpt
    go TypeBool = TypeBool
    go TypeStr = TypeStr
    go TypeUnit = TypeUnit
    go (TypeArrow a b) = TypeArrow (go a) (go b)
    go (TypeMeta meta) = case subst meta of
        Just t -> t
        Nothing -> TypeMeta meta

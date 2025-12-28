module Reussir.Core.Tyck where

import Effectful (Eff)
import Reussir.Core.Types.Expr qualified as Sem
import Reussir.Core.Types.Type qualified as Sem
import Reussir.Parser.Types.Expr qualified as Syn

type Tyck = Eff '[] -- TODO: Define effects used in type checking

inferType :: Syn.Expr -> Tyck (Sem.Type, Sem.Expr)
inferType (Syn.ConstExpr (Syn.ConstInt _)) = undefined
inferType _ = undefined

checkType :: Syn.Expr -> Sem.Type -> Tyck Sem.Expr
checkType expr ty = undefined

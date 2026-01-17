module Reussir.Core2.Data.Operator where

data ArithOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    deriving (Show, Eq)

data CmpOp = Lt | Gt | Lte | Gte | Equ | Neq deriving (Show, Eq)

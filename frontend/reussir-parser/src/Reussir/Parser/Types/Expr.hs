module Reussir.Parser.Types.Expr where

import Data.Int (Int64)
import Data.List
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Reussir.Parser.Types.Lexer (Identifier (..), Path, WithSpan)
import Reussir.Parser.Types.Type (Type)

data Pattern = Pattern Identifier Identifier [Identifier] deriving (Eq)

instance Show Pattern where
    show (Pattern ns name es) = show ns ++ "::" ++ show name ++ "(" ++ intercalate ", " (map show es) ++ ")"

data Constant
    = ConstInt Int
    | ConstDouble Scientific
    | ConstString T.Text
    | ConstBool Bool
    deriving (Show, Eq)

data BinaryOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Lt
    | Gt
    | Lte
    | Gte
    | Equ
    | Neq
    | And
    | Or
    deriving (Show, Eq)
data UnaryOp = Negate | Not deriving (Show, Eq)

data CtorCall = CtorCall
    { ctorName :: Path
    , ctorVariant :: Maybe Identifier -- For enum variants
    , ctorTyArgs :: [Maybe Type] -- underscore represented as Nothing
    , ctorArgs :: [(Maybe Identifier, Expr)]
    }
    deriving (Show, Eq)

data FuncCall = FuncCall
    { funcCallName :: Path
    , funcCallTyArgs :: [Maybe Type]
    , funcCallArgs :: [Expr]
    }
    deriving (Show, Eq)

data Access
    = Named Identifier
    | Unnamed Int64
    deriving (Show, Eq)

type FlexFlag = Bool

data Expr
    = ConstExpr Constant
    | BinOpExpr BinaryOp Expr Expr
    | UnaryOpExpr UnaryOp Expr
    | If Expr Expr Expr
    | Cast Type Expr
    | LetIn (WithSpan Identifier) (Maybe (Type, FlexFlag)) Expr Expr
    | Lambda Identifier Type Expr
    | Match Expr [(Pattern, Expr)]
    | Var Path
    | FuncCallExpr FuncCall -- foo<i32, _> (arg1, arg2) -- notice the difference between variant ctor calls
    | RegionalExpr Expr -- regional { ... }
    | CtorCallExpr CtorCall -- std::Foo {1, 2} / Foo<i32> {x: 1, y: 2} / List<i32>::Nil / List<i32>::Cons(1, xs)
    | AccessChain Expr [Access] -- foo.bar.baz.0.1
    | SpannedExpr (WithSpan Expr)
    deriving (Show, Eq)

module Reussir.Parser.Types.Expr where

import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Data.Vector.Strict (Vector)
import Reussir.Parser.Types.Lexer (Identifier (..), Path, WithSpan)
import Reussir.Parser.Types.Type (Type)
import Data.Vector qualified as V

-- pattern ::= pattern-kind (`if` expr)?
data Pattern = Pattern { patKind :: PatternKind, patGuard :: Maybe Expr } deriving (Eq, Show)

-- pattern-kind ::=
--   | `_`
--   | `x`
--   | path `{` (named-pattern),+  (`..`)? `}`
--   | path `(` (pattern-kind),+  (`..`)? `)`
--   | constant

-- named-pattern ::= identifier (`:`  pattern-kind)? -- syntax sugar for direct binding with identical field name
data PatternKind
    = WildcardPat 
    | BindPat Identifier
    | CtorPat 
        { patCtorPath :: Path
        , patCtorArgs :: V.Vector PatternCtorArg
        , patCtorHasEllipsis :: Bool 
        , patCtorIsNamed :: Bool -- FIXME: did AI add this?
        }
    | ConstPat Constant
     deriving (Eq, Show)

data PatternCtorArg
    = PatternCtorArg -- TODO: seems that there is something off here. we shouldn't need to record ctor is named or not
    { patCtorArgField :: Maybe Identifier
    , patCtorArgKind :: PatternKind 
    }
    deriving (Eq, Show)

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
    | Let (WithSpan Identifier) (Maybe (Type, FlexFlag)) Expr
    | ExprSeq [Expr]
    | Lambda Identifier Type Expr
    | Match Expr (Vector (Pattern, Expr))
    | Var Path
    | FuncCallExpr FuncCall -- foo<i32, _> (arg1, arg2) -- notice the difference between variant ctor calls
    | RegionalExpr Expr -- regional { ... }
    | CtorCallExpr CtorCall -- std::Foo {1, 2} / Foo<i32> {x: 1, y: 2} / List<i32>::Nil / List<i32>::Cons(1, xs)
    | AccessChain Expr (Vector Access) -- foo.bar.baz.0.1
    | SpannedExpr (WithSpan Expr)
    | -- regional assignment { x->y := z }
      -- where x is a flex regional object, y is a mutable field and z is an expression evaluated
      -- to a nullable regional object
      Assign Expr Access Expr
    deriving (Show, Eq)

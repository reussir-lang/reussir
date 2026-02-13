module Reussir.Parser.Types.Stmt where

import Data.Vector.Strict qualified as V

import Reussir.Parser.Types.Capability (Capability)
import Reussir.Parser.Types.Expr (Expr)
import Reussir.Parser.Types.Lexer (Identifier, Path, WithSpan)
import Reussir.Parser.Types.Type (Type)
import qualified Data.Text as T

data Visibility = Public | Private deriving (Show, Eq)

type FieldFlag = Bool
type FlexFlag = Bool

data RecordFields
    = Named (V.Vector (WithSpan (Identifier, Type, FieldFlag)))
    | Unnamed (V.Vector (WithSpan (Type, FieldFlag)))
    | Variants (V.Vector (WithSpan (Identifier, V.Vector Type)))
    deriving (Show, Eq)

data RecordKind = StructKind | EnumKind deriving (Show, Eq)

data Record = Record
    { recordName :: Identifier
    , recordTyParams :: [(Identifier, [Path])]
    , recordFields :: RecordFields
    , recordKind :: RecordKind
    , recordVisibility :: Visibility
    , recordDefaultCap :: Capability
    }
    deriving (Show, Eq)

data Function = Function
    { funcVisibility :: Visibility
    , funcName :: Identifier
    , funcGenerics :: [(Identifier, [Path])]
    , funcParams :: [(Identifier, Type, FlexFlag)]
    , funcReturnType :: Maybe (Type, FlexFlag)
    , funcIsRegional :: Bool
    , funcBody :: Maybe Expr
    }
    deriving (Show, Eq)

data Stmt
    = FunctionStmt Function
    | RecordStmt Record
    | ExternTrampolineStmt {
        etsName :: Identifier,
        etsABI :: T.Text,
        etsFunc :: Path
    }
    | SpannedStmt (WithSpan Stmt)
    deriving (Show, Eq)

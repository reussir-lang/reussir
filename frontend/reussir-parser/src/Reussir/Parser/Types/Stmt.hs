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

-- | Direction of an FFI declaration.
data FFIDirection = FFIExport | FFIImport deriving (Show, Eq)

-- | Body of an FFI declaration.
data FFIBody
    = -- | Export alias: @= path\<type_args\>@, wraps an existing Reussir function.
      FFIAlias Path [Type]
    | -- | Quoted template: @{ \`\`\`...template...\`\`\` }@, inline foreign source code.
      FFITemplate T.Text
    | -- | Simple extern declaration with no body (semicolon-terminated).
      FFIExtern
    deriving (Show, Eq)

data Stmt
    = FunctionStmt Function
    | RecordStmt Record
    | -- | FFI declaration supporting both import and export directions.
      --
      -- Syntax examples:
      --
      -- Legacy: @extern \"C\" trampoline \"bar\" = foo\<i32\>;@
      --
      -- Import: @extern \"C\" import fn strlen(s: i64) -> i64;@
      --
      -- Export: @extern \"C\" export fn bar = foo\<i32\>;@
      --
      -- Import with template:
      -- @extern \"C\" import fn push\<T\>(v: Vec\<T\>, e: T) -> Vec\<T\> { \`\`\`...template...\`\`\` }@
      ExternFFIStmt {
        efsABI :: T.Text,
        efsDirection :: FFIDirection,
        efsName :: Identifier,
        efsGenerics :: [(Identifier, [Path])],
        efsParams :: [(Identifier, Type)],
        efsReturnType :: Maybe Type,
        efsBody :: FFIBody
    }
    | -- | Extern struct declaration for opaque FFI types.
      --
      -- Syntax: @extern struct Vec\<T\> = \"::reussir_rt::collections::vec::Vec\<${T}\>\";@
      --
      -- Declares an opaque type always behind RC, mapped to a foreign type via template.
      ExternStructStmt {
        essName :: Identifier,
        essGenerics :: [(Identifier, [Path])],
        essForeignType :: T.Text
    }
    | ModStmt Visibility Identifier
    | SpannedStmt (WithSpan Stmt)
    deriving (Show, Eq)

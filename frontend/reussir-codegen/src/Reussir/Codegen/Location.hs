{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Location where

import Data.Int (Int64)
import Data.String (IsString (..))
import Data.Text qualified as T
import Reussir.Codegen.Context.Symbol (Symbol)
import Reussir.Codegen.Type.Data (PrimitiveInt)

-- We cannot directly emit LLVM DINode here as certain type may require
-- layout information.
-- In order to workaround the issue, we add frontend and backend attributes
-- to represent debug information. These attributes will be transformed properly
-- to LLVM DI attributes in the backend codegen phase.
-- TODO: need to support more types, for now, we can skip the debug info if
-- unsupported types are encountered.
data DBGType
    = Record
        { dbgRecordName :: T.Text -- unmangled record name
        , dbgRecordFields :: [(T.Text, DBGType)] -- record field name and type
        , dbgRecordTyParams :: [DBGType] -- type parameters
        , dbgRecordRep :: Symbol -- actually backend type attribute
        , dbgRecordIsVariant :: Bool
        }
    | Signed PrimitiveInt
    | Unsigned PrimitiveInt
    deriving (Show)

data DGBMetaInfo
    = DBGFunction
        { dbgFuncName :: T.Text
        , dbgFuncTyParams :: [DBGType]
        }
    | DBGLocalVar
        { dbgLocalVarName :: T.Text
        , dbgLocalVarTy :: DBGType
        }
    | DBGGlobalVar
        { dbgGlobalVarName :: T.Text
        , dbgGlobalVarTy :: DBGType
        }
    | DBGRawMeta T.Text -- for future extension
    deriving (Show)

data Location
    = CallSiteLoc {callee :: Location, caller :: Location}
    | FileLineColRange
        { filename :: T.Text
        , startLine :: Int64
        , startColumn :: Int64
        , endLine :: Int64
        , endColumn :: Int64
        }
    | FusedLoc
        { metadata :: Maybe DGBMetaInfo -- currently only used text for meta
        , locations :: [Location]
        }
    | UnknownLoc
    | NameLoc
        { locName :: T.Text
        , childLoc :: Maybe Location
        }
    deriving (Show)

instance IsString DGBMetaInfo where
    fromString s = DBGRawMeta (T.pack s)

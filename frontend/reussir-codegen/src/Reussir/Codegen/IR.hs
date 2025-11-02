module Reussir.Codegen.IR where

import Data.Text.Lazy qualified as T
import Reussir.Codegen.Context (Path)
import Reussir.Codegen.Intrinsics (IntrinsicCall)
import Reussir.Codegen.Location (Location)
import Reussir.Codegen.Value (TypedValue)

data FuncCall = FuncCall
    { target :: Path
    , args :: [TypedValue]
    , results :: [TypedValue]
    }
    deriving (Eq, Show)

data Instr
    = ICall IntrinsicCall
    | FCall FuncCall
    | Panic T.Text
    | Return (Maybe TypedValue)
    | WithLoc Location Instr
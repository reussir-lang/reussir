module Reussir.Core.Data.Full.Error where

import Data.Int (Int64)
import Data.Vector.Strict qualified as V

import Reussir.Core.Data.Semi.Type qualified as Semi
import Reussir.Core.Data.UniqueID (GenericID)
import Reussir.Parser.Types.Capability qualified as Syn
import Reussir.Parser.Types.Lexer (Path (..))

data Error = Error
    { errorSpan :: (Int64, Int64)
    , errorKind :: ErrorKind
    }

data ErrorKind
    = InvalidRecordField
        { recordPath :: Path -- The path of the record
        , recordTypeArgs :: V.Vector Semi.Type -- The type arguments of the record
        , errorIndex :: Int -- The index of the field that is invalid
        }
    | InvalidNullableType Semi.Type
    | UnknownGeneric GenericID
    | InvalidCapability Path Syn.Capability

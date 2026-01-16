module Reussir.Core2.Types.Semi.Unification where

import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful.Prim.IORef.Strict (IORef')
import Reussir.Core2.Types.Class (TypeBound)
import Reussir.Core2.Types.Semi.Type (HoleID, Type)

data UnificationState
    = UnSolvedUFRoot {-# UNPACK #-} !Int TypeBound
    | SolvedUFRoot !Int Type
    | UFNode {-# UNPACK #-} !HoleID

data HoleState = HoleState
    { holeName :: Maybe T.Text
    , holeSpan :: Maybe (Int64, Int64)
    , holeUnification :: IORef' UnificationState
    }

newtype HoleTable = HoleTable
    { holes :: IORef' (Seq.Seq HoleState)
    }

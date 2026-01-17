module Reussir.Core2.Types.Semi.Unification where

import Data.Int (Int64)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful (Eff, IOE)
import Effectful.Prim.IORef.Strict (IORef', Prim)
import Effectful.Reader.Static (Reader)
import Reussir.Core2.Types.Class (ClassDAG, TypeBound)
import Reussir.Core2.Types.Generic (GenericState)
import Reussir.Core2.Types.Semi.Type (HoleID, Type, TypeClassTable)

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

data ErrorKind
    = URMisMatchedBounds
        { candidateType :: Type
        , candidateBounds :: TypeBound
        , candidateHoleSpan :: Maybe (Int64, Int64)
        , candidateHoleName :: Maybe T.Text
        }
    | URMisMatchedType Type Type

data Failure = Failure
    { errorKind :: ErrorKind
    , unificationContext :: T.Text
    , innerFailures :: [Failure]
    }

type UnificationEff =
    Eff
        '[ IOE
         , Prim
         , Reader HoleTable
         , Reader ClassDAG
         , Reader TypeClassTable
         , Reader GenericState
         ]

module Reussir.Core.Semi.PatternMatch where

import Reussir.Parser.Types.Expr qualified as Syn
import Reussir.Parser.Types.Lexer (Path)
import qualified Data.Vector.Strict as V
import Reussir.Core.Data.Semi.Context (SemiEff)
import Reussir.Core.Data.Semi.Expr (DecisionTree, Expr)

-- TODO: implement this after syntax parsing is fixed.
-- normalize a ctor pattern into a positional applied form.
-- fill in wildcards for ignored fields if ellipsis is present.
-- return Nothing if the normalization fail
normalizeCtorPattern 
    :: Path 
    -> V.Vector Syn.PatternCtorArg 
    -> Bool 
    -> SemiEff (Maybe (V.Vector Syn.PatternKind))
normalizeCtorPattern recordPath args hasEllipsis = undefined

-- elaborate a pattern into a decision within the semi-elaborated expression space.
-- return Nothing if the elaboration fail
patternToDecisionTree :: Syn.Pattern -> SemiEff (Maybe (DecisionTree Expr))
patternToDecisionTree = undefined

-- TODO: scaffold data structure used for exhaustive checking.

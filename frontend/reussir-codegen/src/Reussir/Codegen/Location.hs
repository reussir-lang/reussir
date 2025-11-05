{-# LANGUAGE OverloadedStrings #-}

module Reussir.Codegen.Location where

import Data.Int (Int64)
import Data.Text qualified as T

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
        { metadata :: Maybe T.Text -- currently only used text for meta
        , locations :: [Location]
        }
    | UnknownLoc
    | NameLoc
        { locName :: T.Text
        , childLoc :: Maybe Location
        }
    deriving (Show)

module Reussir.Parser.Types.Capability where

data Capability
    = Unspecified
    | Shared
    | Value
    | Flex
    | Rigid
    | Field
    deriving (Show, Eq)

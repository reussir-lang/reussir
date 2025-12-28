module Reussir.Core where

import Data.Text qualified as T

data ModuleConfig = ModuleConfig
    { moduleName :: T.Text
    , modulePath :: FilePath
    }
    deriving (Show, Eq)

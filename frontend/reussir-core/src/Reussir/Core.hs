module Reussir.Core where

import Data.Int (Int64)
import Data.Text qualified as T

data ModuleConfig = ModuleConfig
    { moduleName :: T.Text
    , modulePath :: FilePath
    }
    deriving (Show, Eq)

data TranslationState = TranslationState
    { currentSpan :: (Int64, Int64)
    }
    deriving (Show, Eq)

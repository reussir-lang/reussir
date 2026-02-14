module Reussir.Core.Semi.Trampoline where
import Reussir.Parser.Types.Lexer (Identifier, Path)
import qualified Data.Text as T
import qualified Reussir.Parser.Types.Type as Syn
import Reussir.Core.Data.Semi.Context (SemiEff)

resolveTrampoline :: 
    Identifier ->
    T.Text ->
    Path ->
    [Syn.Type] ->
    SemiEff ()
resolveTrampoline name abi path args = 
    -- check and verify the function is defined and the args can be applied
    -- emit error if needed
    -- if everything checks, register the trampoline to context
    undefined

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- Base libraries
import Control.Exception (SomeException, bracketOnError, catch)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.String (IsString (fromString))
import Foreign (FunPtr, nullPtr)
import Foreign.Ptr (castPtrToFunPtr)

-- Text
import Data.Text qualified as T

-- Megaparsec
import Text.Megaparsec (errorBundlePretty, runParser)

-- Console
import System.Console.Haskeline
import System.Console.Haskeline.IO

-- Reussir
import Reussir.Bridge (LogLevel (..), OptOption (..), ReussirJIT, addModule, lookupSymbol, withJIT)
import Reussir.Core.REPL (ReplState, initReplState, addDefinition, compileExpression, ReplError(..))
import Reussir.Parser.Prog (ReplInput(..), parseReplInput)

-- | Placeholder callback for lazy module loading
-- This would be used if we want to do lazy compilation of ASTs
placeholderCallback :: () -> IO ByteString
placeholderCallback _ = return ""

foreign import ccall "dynamic"
    callU64Func :: FunPtr (IO Int64) -> IO Int64

-- | Main REPL entry point
main :: IO ()
main =
    bracketOnError
        (initializeInput defaultSettings)
        cancelInput
        (\hd -> do
            state <- initReplState LogWarning "<repl>"
            -- Use () as the AST type since we're not using lazy modules
            withJIT placeholderCallback OptTPDE $ \jit ->
                loop jit state 0 hd >> closeInput hd
        )
  where
    loop :: ReussirJIT () -> ReplState -> Int64 -> InputState -> IO ()
    loop jit state n hd = do
        minput <- queryInput hd (getInputLine "λ> ")
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just ":q" -> return ()
            Just "" -> loop jit state n hd
            Just input -> do
                result <- processInput jit state n input
                case result of
                    Left errMsg -> do
                        queryInput hd $ outputStrLn errMsg
                        loop jit state n hd
                    Right (output, state') -> do
                        case output of
                            Just msg -> queryInput hd $ outputStrLn msg
                            Nothing -> return ()
                        loop jit state' (n + 1) hd

-- | Process a single line of REPL input
processInput ::
    ReussirJIT () ->
    ReplState ->
    Int64 ->
    String ->
    IO (Either String (Maybe String, ReplState))
processInput jit state n input = do
    catch
        ( case runParser parseReplInput "<repl>" (T.pack input) of
            Left err -> return $ Left $ errorBundlePretty err
            Right replInput -> case replInput of
                ReplStmt stmt -> do
                    result <- addDefinition state stmt
                    case result of
                        Left (TypeCheckError msg) -> return $ Left $ "Type error: " ++ msg
                        Left (ParseError msg) -> return $ Left $ "Parse error: " ++ msg
                        Left (CompilationError msg) -> return $ Left $ "Compilation error: " ++ msg
                        Right state' -> return $ Right (Just "Definition added.", state')
                ReplExpr expr -> do
                    result <- compileExpression state expr
                    case result of
                        Left (TypeCheckError msg) -> return $ Left $ "Type error: " ++ msg
                        Left (ParseError msg) -> return $ Left $ "Parse error: " ++ msg
                        Left (CompilationError msg) -> return $ Left $ "Compilation error: " ++ msg
                        Right (moduleBytes, state') -> do
                            -- JIT compile and execute
                            executeResult <- executeModule jit moduleBytes n
                            case executeResult of
                                Left msg -> return $ Left msg
                                Right resultStr -> return $ Right (Just resultStr, state')
        )
        (\(e :: SomeException) -> return $ Left $ "Error: " ++ show e)

-- | Execute a compiled module in the JIT and return the result
executeModule :: ReussirJIT () -> ByteString -> Int64 -> IO (Either String String)
executeModule jit moduleBytes n = do
    let funcName = "__repl_expr_" ++ show n
    let packedName = fromString funcName
    flag <- addModule jit moduleBytes
    if flag
        then do
            sym <- lookupSymbol jit packedName False
            if sym /= nullPtr
                then do
                    -- For now, assume result is u64
                    result <- callU64Func (castPtrToFunPtr sym)
                    return $ Right $ show result
                else return $ Left $ "Symbol not found: " ++ funcName
        else return $ Left "Failed to add module to JIT"

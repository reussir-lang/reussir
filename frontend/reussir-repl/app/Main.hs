{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- Base libraries
import Control.Exception (SomeException, bracketOnError, catch)
import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.String (IsString (fromString))
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign (FunPtr, Ptr, nullPtr)
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
import Reussir.Core.REPL (
    ReplError (..),
    ReplMode (..),
    ReplState (..),
    ResultKind (..),
    addDefinition,
    compileExpression,
    getReplMode,
    initReplState,
    setReplMode,
 )
import Reussir.Parser.Expr (parseExpr)
import Reussir.Parser.Prog (ReplInput (..), parseReplInput)
import Reussir.Parser.Stmt (parseStmt)
import Reussir.Parser.Types.Expr qualified as P

--------------------------------------------------------------------------------
-- Foreign function imports for different result types
--------------------------------------------------------------------------------

foreign import ccall "dynamic"
    callI64Func :: FunPtr (IO Int64) -> IO Int64

foreign import ccall "dynamic"
    callI32Func :: FunPtr (IO Int32) -> IO Int32

foreign import ccall "dynamic"
    callI16Func :: FunPtr (IO Int16) -> IO Int16

foreign import ccall "dynamic"
    callI8Func :: FunPtr (IO Int8) -> IO Int8

foreign import ccall "dynamic"
    callU64Func :: FunPtr (IO Word64) -> IO Word64

foreign import ccall "dynamic"
    callU32Func :: FunPtr (IO Word32) -> IO Word32

foreign import ccall "dynamic"
    callU16Func :: FunPtr (IO Word16) -> IO Word16

foreign import ccall "dynamic"
    callU8Func :: FunPtr (IO Word8) -> IO Word8

foreign import ccall "dynamic"
    callF64Func :: FunPtr (IO Double) -> IO Double

foreign import ccall "dynamic"
    callF32Func :: FunPtr (IO Float) -> IO Float

foreign import ccall "dynamic"
    callBoolFunc :: FunPtr (IO Word8) -> IO Word8

foreign import ccall "dynamic"
    callUnitFunc :: FunPtr (IO ()) -> IO ()

--------------------------------------------------------------------------------
-- Placeholder callback for lazy module loading
--------------------------------------------------------------------------------

{- | Placeholder callback for lazy module loading
This would be used if we want to do lazy compilation of ASTs
-}
placeholderCallback :: () -> IO ByteString
placeholderCallback _ = return ""

--------------------------------------------------------------------------------
-- Main REPL Entry Point
--------------------------------------------------------------------------------

-- | Main REPL entry point
main :: IO ()
main =
    bracketOnError
        (initializeInput defaultSettings)
        cancelInput
        ( \hd -> do
            putStrLn "Reussir REPL v0.1.0"
            putStrLn "Type :help for available commands, :q to quit"
            putStrLn ""
            state <- initReplState LogWarning "<repl>"
            -- Use () as the AST type since we're not using lazy modules
            -- Note: Using OptDefault instead of OptTPDE due to a bug in TPDE with function calls
            withJIT placeholderCallback OptDefault $ \jit ->
                loop jit state hd >> closeInput hd
        )

--------------------------------------------------------------------------------
-- Command Handling
--------------------------------------------------------------------------------

-- | Check if input is a REPL command
isCommand :: String -> Bool
isCommand (':':_) = True
isCommand _ = False

-- | Process REPL commands
processCommand ::
    ReussirJIT () ->
    ReplState ->
    String ->
    InputState ->
    IO (Maybe ReplState)
processCommand _jit state cmd hd = case words cmd of
    [":q"] -> return Nothing
    [":quit"] -> return Nothing
    [":help"] -> do
        queryInput hd $ outputStrLn helpText
        return $ Just state
    [":mode"] -> do
        let mode = getReplMode state
        queryInput hd $ outputStrLn $ "Current mode: " ++ show mode
        return $ Just state
    [":stmt"] -> do
        queryInput hd $ outputStrLn "Switched to statement mode"
        return $ Just $ setReplMode StmtMode state
    [":eval"] -> do
        queryInput hd $ outputStrLn "Switched to eval mode"
        return $ Just $ setReplMode EvalMode state
    [":clear"] -> do
        -- Re-initialize the REPL state
        newState <- initReplState (replLogLevel state) (replFilePath state)
        queryInput hd $ outputStrLn "Context cleared"
        return $ Just newState
    _ -> do
        queryInput hd $ outputStrLn $ "Unknown command: " ++ cmd
        queryInput hd $ outputStrLn "Type :help for available commands"
        return $ Just state

-- | Help text for REPL commands
helpText :: String
helpText = unlines
    [ "Available commands:"
    , "  :help      Show this help message"
    , "  :q, :quit  Exit the REPL"
    , "  :stmt      Switch to statement mode (add definitions)"
    , "  :eval      Switch to eval mode (evaluate expressions)"
    , "  :mode      Show current mode"
    , "  :clear     Clear the context and start fresh"
    , ""
    , "In statement mode, input is parsed as function/record definitions."
    , "In eval mode, input is parsed as expressions to evaluate."
    ]

--------------------------------------------------------------------------------
-- Main REPL Loop
--------------------------------------------------------------------------------

-- | The main REPL loop
loop :: ReussirJIT () -> ReplState -> InputState -> IO ()
loop jit state hd = do
    let prompt = case getReplMode state of
            StmtMode -> "λ> "
            EvalMode -> "ε> "
    minput <- queryInput hd (getInputLine prompt)
    case minput of
        Nothing -> return ()
        Just "" -> loop jit state hd
        Just input
            | isCommand input -> do
                mState <- processCommand jit state input hd
                case mState of
                    Nothing -> return ()  -- Exit
                    Just state' -> loop jit state' hd
            | otherwise -> do
                result <- processInput jit state input
                case result of
                    Left errMsg -> do
                        queryInput hd $ outputStrLn errMsg
                        loop jit state hd
                    Right (output, state') -> do
                        case output of
                            Just msg -> queryInput hd $ outputStrLn msg
                            Nothing -> return ()
                        loop jit state' hd

--------------------------------------------------------------------------------
-- Input Processing
--------------------------------------------------------------------------------

-- | Process a single line of REPL input based on current mode
processInput ::
    ReussirJIT () ->
    ReplState ->
    String ->
    IO (Either String (Maybe String, ReplState))
processInput jit state input = do
    catch
        ( case getReplMode state of
            StmtMode -> processAutoDetect jit state input
            EvalMode -> processExpression jit state input
        )
        (\(e :: SomeException) -> return $ Left $ "Error: " ++ show e)

-- | Auto-detect whether input is a statement or expression
processAutoDetect ::
    ReussirJIT () ->
    ReplState ->
    String ->
    IO (Either String (Maybe String, ReplState))
processAutoDetect jit state input = do
    case runParser parseReplInput "<repl>" (T.pack input) of
        Left err -> return $ Left $ errorBundlePretty err
        Right (ReplStmt stmt) -> do
            result <- addDefinition state stmt
            case result of
                Left (TypeCheckError msg) -> return $ Left $ "Type error: " ++ msg
                Left (ParseError msg) -> return $ Left $ "Parse error: " ++ msg
                Left (CompilationError msg) -> return $ Left $ "Compilation error: " ++ msg
                Left (ElaborationError msg) -> return $ Left $ "Elaboration error: " ++ msg
                Right state' -> return $ Right (Just "Definition added.", state')
        Right (ReplExpr expr) -> do
            processExpressionParsed jit state input expr

-- | Process input as an expression (evaluate)
processExpression ::
    ReussirJIT () ->
    ReplState ->
    String ->
    IO (Either String (Maybe String, ReplState))
processExpression jit state input = do
    case runParser parseExpr "<repl>" (T.pack input) of
        Left err -> return $ Left $ errorBundlePretty err
        Right expr -> processExpressionParsed jit state input expr

-- | Process a pre-parsed expression
processExpressionParsed ::
    ReussirJIT () ->
    ReplState ->
    String ->
    P.Expr ->
    IO (Either String (Maybe String, ReplState))
processExpressionParsed jit state input expr = do
    result <- compileExpression state (T.pack input) expr
    case result of
        Left (TypeCheckError msg) -> return $ Left $ "Type error: " ++ msg
        Left (ParseError msg) -> return $ Left $ "Parse error: " ++ msg
        Left (CompilationError msg) -> return $ Left $ "Compilation error: " ++ msg
        Left (ElaborationError msg) -> return $ Left $ "Elaboration error: " ++ msg
        Right (moduleBytes, state', resultKind) -> do
            -- JIT compile and execute
            -- Use replCounter - 1 since compileExpression increments after generating the name
            let exprCounter = fromIntegral (replCounter state' - 1)
            executeResult <- executeModule jit moduleBytes exprCounter resultKind
            case executeResult of
                Left msg -> return $ Left msg
                Right resultStr -> return $ Right (Just resultStr, state')

--------------------------------------------------------------------------------
-- JIT Execution
--------------------------------------------------------------------------------

-- | Execute a compiled module in the JIT and return the result
executeModule :: 
    ReussirJIT () -> 
    ByteString -> 
    Int64 -> 
    ResultKind -> 
    IO (Either String String)
executeModule jit moduleBytes n resultKind = do
    let funcName = "__repl_expr_" ++ show n
    let packedName = fromString funcName
    flag <- addModule jit moduleBytes
    if flag
        then do
            sym <- lookupSymbol jit packedName False
            if sym /= nullPtr
                then do
                    result <- executeWithResultKind sym resultKind
                    return $ Right result
                else return $ Left $ "Symbol not found: " ++ funcName
        else return $ Left "Failed to add module to JIT"

-- | Execute a function pointer based on the result kind and format output
executeWithResultKind :: Ptr () -> ResultKind -> IO String
executeWithResultKind sym resultKind = case resultKind of
    ResultI8 -> do
        result <- callI8Func (castPtrToFunPtr sym)
        return $ show result ++ " : i8"
    ResultI16 -> do
        result <- callI16Func (castPtrToFunPtr sym)
        return $ show result ++ " : i16"
    ResultI32 -> do
        result <- callI32Func (castPtrToFunPtr sym)
        return $ show result ++ " : i32"
    ResultI64 -> do
        result <- callI64Func (castPtrToFunPtr sym)
        return $ show result ++ " : i64"
    ResultU8 -> do
        result <- callU8Func (castPtrToFunPtr sym)
        return $ show result ++ " : u8"
    ResultU16 -> do
        result <- callU16Func (castPtrToFunPtr sym)
        return $ show result ++ " : u16"
    ResultU32 -> do
        result <- callU32Func (castPtrToFunPtr sym)
        return $ show result ++ " : u32"
    ResultU64 -> do
        result <- callU64Func (castPtrToFunPtr sym)
        return $ show result ++ " : u64"
    ResultF16 -> do
        -- F16 is typically not directly supported in C ABI, use F32
        result <- callF32Func (castPtrToFunPtr sym)
        return $ show result ++ " : f16"
    ResultF32 -> do
        result <- callF32Func (castPtrToFunPtr sym)
        return $ show result ++ " : f32"
    ResultF64 -> do
        result <- callF64Func (castPtrToFunPtr sym)
        return $ show result ++ " : f64"
    ResultBool -> do
        result <- callBoolFunc (castPtrToFunPtr sym)
        return $ (if result /= 0 then "true" else "false") ++ " : bool"
    ResultUnit -> do
        callUnitFunc (castPtrToFunPtr sym)
        return "()"
    ResultOther tyName -> do
        -- For non-primitive types, we can't easily print the value
        -- Just indicate the expression was evaluated
        _ <- callI64Func (castPtrToFunPtr sym)  -- Execute to trigger side effects
        return $ "expr evaluated as " ++ T.unpack tyName

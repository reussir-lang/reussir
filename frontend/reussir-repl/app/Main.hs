{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- Base libraries
import Control.Exception (SomeException, bracketOnError, catch)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.String (IsString (fromString))
import Foreign (FunPtr)
import Foreign.Ptr (castPtrToFunPtr)

-- Text
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- Effectful & Logging
import Effectful qualified as E
import Effectful.Log (defaultLogLevel)
import Effectful.Log qualified as L
import Log.Backend.StandardOutput qualified as L

-- Console
import System.Console.Haskeline
import System.Console.Haskeline.IO

-- Reussir
import Reussir.Bridge (LogLevel (..), OptOption (..), OutputTarget (..), ReussirJIT, addModule, lookupSymbol, withJIT)
import Reussir.Codegen (Module (..), emitModuleToText, emptyModule)
import Reussir.Codegen.Context (TargetSpec (..))
import Reussir.Codegen.Context.Symbol (verifiedSymbol)
import Reussir.Codegen.IR qualified as IR
import Reussir.Codegen.Intrinsics qualified as I
import Reussir.Codegen.Type qualified as TT
import Reussir.Codegen.Value qualified as V

data Op = Add | Sub | Mul | Div
    deriving (Show)

data Expr
    = Lit Double
    | Call Op Expr Expr
    deriving (Show)

createSimpleModule :: IR.Function -> Module
createSimpleModule function =
    (emptyModule spec){moduleFunctions = [function]}
  where
    spec = TargetSpec "jit_module" "output.o" OptDefault OutputObject LogWarning

type NamedExpr = (String, Expr)

-- Helper types for codegen
primitiveF64 :: TT.Type
primitiveF64 = TT.TypePrim (TT.PrimFloat TT.PrimFloat64)

val :: Int64 -> V.Value
val = V.Value

typedVal :: Int64 -> TT.Type -> V.TypedValue
typedVal v t = (val v, t)

f64val :: Int64 -> V.TypedValue
f64val v = typedVal v primitiveF64

-- Compile expression to IR instructions
-- Returns (result SSA value, next free SSA value, instructions)
compileExpr :: Expr -> Int64 -> (V.TypedValue, Int64, [IR.Instr])
compileExpr (Lit d) nextSSA =
    let resultSSA = nextSSA
        instr = IR.ICall (I.IntrinsicCall (I.Arith (I.Constant (read (show d)))) [] [f64val resultSSA])
     in (f64val resultSSA, nextSSA + 1, [instr])
compileExpr (Call op e1 e2) nextSSA =
    let (v1, nextSSA1, instrs1) = compileExpr e1 nextSSA
        (v2, nextSSA2, instrs2) = compileExpr e2 nextSSA1
        resultSSA = nextSSA2
        opInstr = case op of
            Add -> I.Arith (I.Addf (I.FastMathFlag 0))
            Sub -> I.Arith (I.Subf (I.FastMathFlag 0))
            Mul -> I.Arith (I.Mulf (I.FastMathFlag 0))
            Div -> I.Arith (I.Divf (I.FastMathFlag 0))
        callInstr = IR.ICall (I.IntrinsicCall opInstr [v1, v2] [f64val resultSSA])
     in (f64val resultSSA, nextSSA2 + 1, instrs1 ++ instrs2 ++ [callInstr])

-- Create a function () -> Double of given name for the given expression
exprToFunction :: String -> Expr -> IR.Function
exprToFunction name expr =
    let (resultVal, _, instrs) = compileExpr expr 0
        returnInstr = IR.Return (Just resultVal)
     in IR.Function
            { IR.funcLinkage = IR.LnkExternal
            , IR.funcLLVMVisibility = IR.LLVMVisDefault
            , IR.funcMLIRVisibility = IR.MLIRVisPublic
            , IR.funcSymbol = verifiedSymbol (T.pack name)
            , IR.funcArgs = []
            , IR.funcResult = primitiveF64
            , IR.funcLoc = Nothing
            , IR.funcBody =
                Just
                    ( IR.Block
                        { IR.blkArgs = []
                        , IR.blkBody = instrs ++ [returnInstr]
                        }
                    )
            }

exprToModule :: NamedExpr -> IO ByteString
exprToModule (name, expr) = do
    let function = exprToFunction name expr
    let module' = createSimpleModule function
    result <- L.withStdOutLogger $ \logger -> do
        E.runEff $ L.runLog "Reussir.REPL" logger defaultLogLevel $ emitModuleToText module'
    return $ TE.encodeUtf8 result

tokenize :: String -> [String]
tokenize [] = []
tokenize (c : cs)
    | c `elem` (" \t\n" :: String) = tokenize cs
    | c == '(' = "(" : tokenize cs
    | c == ')' = ")" : tokenize cs
    | otherwise =
        let (token, rest) = span (\x -> x `notElem` (" \t\n()" :: String)) (c : cs)
         in token : tokenize rest

parse :: String -> Expr
parse = fst . parseExpr . tokenize

parseOp :: String -> Op
parseOp "+" = Add
parseOp "-" = Sub
parseOp "*" = Mul
parseOp "/" = Div
parseOp op = error $ "unknown operator: " ++ op

parseExpr :: [String] -> (Expr, [String])
parseExpr ("(" : op : xs) =
    let (arg1, rest1) = parseExpr xs
        (arg2, rest2) = parseExpr rest1
     in case rest2 of
            ")" : rest3 -> (Call (parseOp op) arg1 arg2, rest3)
            _ -> error "expected closing paren"
parseExpr (n : xs) = (Lit (read n), xs)
parseExpr [] = error "unexpected EOF"

foreign import ccall "dynamic"
    getSymbolFunc :: FunPtr (IO Double) -> IO Double

loopAction :: ReussirJIT NamedExpr -> Expr -> Int64 -> IO ()
loopAction jit expr n = do
    let name = "res" ++ show n
    moduleStr <- exprToModule (name, expr)
    let packedName = fromString name
    flag <- addModule jit moduleStr
    unless flag $ error "Failed to add module"
    sym <- lookupSymbol jit packedName False
    result <- getSymbolFunc (castPtrToFunPtr sym)
    putStrLn $ show result

main :: IO ()
main =
    bracketOnError
        (initializeInput defaultSettings)
        cancelInput -- This will only be called if an exception such
        -- as a SigINT is received.
        (\hd -> withJIT exprToModule OptTPDE $ \jit -> loop jit 0 hd >> closeInput hd)
  where
    loop :: ReussirJIT NamedExpr -> Int64 -> InputState -> IO ()
    loop jit n hd = do
        minput <- queryInput hd (getInputLine "λ> ")
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input -> do
                result <-
                    catch
                        ( do
                            let ast = parse input
                            loopAction jit ast n
                            return $ Nothing
                        )
                        ( \(e :: SomeException) ->
                            return $ Just $ "Parse error: " ++ show e
                        )
                case result of
                    Just errMsg -> queryInput hd $ outputStrLn errMsg
                    Nothing -> pure ()
                loop jit (n + 1) hd

module Main (main) where

import Reussir.Bridge (someFunc)

main :: IO ()
main = do
  putStrLn "Running tests..."
  someFunc

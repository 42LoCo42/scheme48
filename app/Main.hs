module Main where

import System.Environment

import Eval
import Read

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ either
    (\e -> "ERROR: " ++ show e)
    show
    (readExpr expr >>= eval)

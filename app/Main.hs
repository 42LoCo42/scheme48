module Main where

import System.Environment

import Control.Monad (when)
import System.IO     (hFlush, stdout)

import Eval
import Read

main :: IO ()
main = do
  args <- getArgs
  if null args
  then runRepl
  else evalAndPrint $ head args

evalString :: String -> String
evalString expr = either
  (\e -> "ERROR: " ++ show e)
  show
  (readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint = flushStr . (++ "\n") . evalString

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

prompt :: String -> IO String
prompt p = flushStr p >> getLine

runRepl :: IO ()
runRepl = do
  expr <- prompt "Lisp> "
  when (expr /= "quit") $ do
    evalAndPrint expr
    runRepl
